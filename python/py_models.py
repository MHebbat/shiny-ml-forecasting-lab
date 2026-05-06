"""
Python model bridge for the Shiny ML app.

Called from R via reticulate (R/python_bridge.R -> py_train_predict).

Each model handles fit-then-predict in a single call so we avoid the round-trip
of trying to serialize fitted models back to R. If you need persistence across
sessions, save a pickle and reload on next call.

Required Python packages (install only the ones you actually use):
    pip install numpy pandas scikit-learn lightgbm catboost
    pip install tensorflow      # for the MLP wrapper
    pip install xgboost         # optional, also exposed
"""
from __future__ import annotations

import numpy as np
import pandas as pd


def _coerce_X(X: pd.DataFrame) -> pd.DataFrame:
    X = X.copy()
    for c in X.columns:
        if X[c].dtype == "object":
            X[c] = X[c].astype("category").cat.codes
    return X.astype(float)


def train_predict(model_id: str,
                  x_train,
                  y_train,
                  x_test,
                  params: dict,
                  task: str = "regression"):
    """Train `model_id` on (x_train, y_train) and predict on x_test.

    Returns dict with keys:
      - predictions: list[float]
      - feature_importance: dict[str, float] | None
    """
    x_train = pd.DataFrame(x_train)
    x_test = pd.DataFrame(x_test)
    y_train = np.asarray(y_train).ravel()
    params = dict(params or {})

    if model_id == "lightgbm":
        return _lightgbm(x_train, y_train, x_test, params, task)
    if model_id == "catboost":
        return _catboost(x_train, y_train, x_test, params, task)
    if model_id == "mlp":
        return _mlp(x_train, y_train, x_test, params, task)
    if model_id == "xgboost":
        return _xgboost(x_train, y_train, x_test, params, task)

    raise ValueError(f"Unknown model_id: {model_id}")


# ---------------------------------------------------------------------
def _lightgbm(X, y, Xt, params, task):
    import lightgbm as lgb
    Xc = _coerce_X(X); Xtc = _coerce_X(Xt)
    if task == "classification":
        clf = lgb.LGBMClassifier(
            n_estimators=int(params.get("n_estimators", 300)),
            learning_rate=float(params.get("learning_rate", 0.05)),
            num_leaves=int(params.get("num_leaves", 31)),
            max_depth=int(params.get("max_depth", -1)),
            verbose=-1)
        clf.fit(Xc, y)
        pred = clf.predict(Xtc)
    else:
        reg = lgb.LGBMRegressor(
            n_estimators=int(params.get("n_estimators", 300)),
            learning_rate=float(params.get("learning_rate", 0.05)),
            num_leaves=int(params.get("num_leaves", 31)),
            max_depth=int(params.get("max_depth", -1)),
            verbose=-1)
        reg.fit(Xc, y)
        pred = reg.predict(Xtc)
        clf = reg
    fi = dict(zip(Xc.columns, getattr(clf, "feature_importances_", [])))
    return {"predictions": pred.tolist(), "feature_importance": fi}


def _catboost(X, y, Xt, params, task):
    from catboost import CatBoostRegressor, CatBoostClassifier
    Xc = _coerce_X(X); Xtc = _coerce_X(Xt)
    common = dict(
        iterations=int(params.get("iterations", 500)),
        learning_rate=float(params.get("learning_rate", 0.05)),
        depth=int(params.get("depth", 6)),
        verbose=False,
    )
    if task == "classification":
        m = CatBoostClassifier(**common)
        m.fit(Xc, y)
        pred = m.predict(Xtc).ravel()
    else:
        m = CatBoostRegressor(**common)
        m.fit(Xc, y)
        pred = m.predict(Xtc)
    fi = dict(zip(Xc.columns, m.get_feature_importance().tolist()))
    return {"predictions": pred.tolist(), "feature_importance": fi}


def _xgboost(X, y, Xt, params, task):
    import xgboost as xgb
    Xc = _coerce_X(X); Xtc = _coerce_X(Xt)
    if task == "classification":
        m = xgb.XGBClassifier(
            n_estimators=int(params.get("n_estimators", 300)),
            learning_rate=float(params.get("learning_rate", 0.1)),
            max_depth=int(params.get("max_depth", 6)),
            verbosity=0)
    else:
        m = xgb.XGBRegressor(
            n_estimators=int(params.get("n_estimators", 300)),
            learning_rate=float(params.get("learning_rate", 0.1)),
            max_depth=int(params.get("max_depth", 6)),
            verbosity=0)
    m.fit(Xc, y)
    pred = m.predict(Xtc)
    fi = dict(zip(Xc.columns, getattr(m, "feature_importances_", [])))
    return {"predictions": pred.tolist(), "feature_importance": fi}


def _mlp(X, y, Xt, params, task):
    """Keras-based MLP. Falls back to sklearn if TF missing."""
    Xc = _coerce_X(X).values.astype("float32")
    Xtc = _coerce_X(Xt).values.astype("float32")
    hidden = [int(h.strip()) for h in str(params.get("hidden", "64,32")).split(",") if h.strip()]
    epochs = int(params.get("epochs", 50))
    batch  = int(params.get("batch_size", 32))
    drop   = float(params.get("dropout", 0.1))
    lr     = float(params.get("learning_rate", 0.001))

    try:
        import tensorflow as tf
        from tensorflow import keras
        from tensorflow.keras import layers
        n_classes = None
        if task == "classification":
            classes, y_idx = np.unique(y, return_inverse=True)
            n_classes = len(classes)
            y_use = y_idx
        else:
            y_use = y.astype("float32")
        inp = keras.Input(shape=(Xc.shape[1],))
        z = inp
        for h in hidden:
            z = layers.Dense(h, activation="relu")(z)
            if drop > 0: z = layers.Dropout(drop)(z)
        if task == "classification":
            out = layers.Dense(n_classes, activation="softmax")(z)
            model = keras.Model(inp, out)
            model.compile(optimizer=keras.optimizers.Adam(lr),
                          loss="sparse_categorical_crossentropy",
                          metrics=["accuracy"])
        else:
            out = layers.Dense(1)(z)
            model = keras.Model(inp, out)
            model.compile(optimizer=keras.optimizers.Adam(lr), loss="mse")
        model.fit(Xc, y_use, epochs=epochs, batch_size=batch, verbose=0)
        pred = model.predict(Xtc, verbose=0)
        if task == "classification":
            pred = classes[np.argmax(pred, axis=1)]
        else:
            pred = pred.ravel()
        return {"predictions": pred.tolist(), "feature_importance": None}
    except Exception:
        # Fallback to sklearn
        from sklearn.neural_network import MLPRegressor, MLPClassifier
        if task == "classification":
            m = MLPClassifier(hidden_layer_sizes=tuple(hidden),
                              max_iter=epochs, batch_size=batch,
                              learning_rate_init=lr)
        else:
            m = MLPRegressor(hidden_layer_sizes=tuple(hidden),
                              max_iter=epochs, batch_size=batch,
                              learning_rate_init=lr)
        m.fit(Xc, y)
        pred = m.predict(Xtc)
        return {"predictions": pred.tolist(), "feature_importance": None}
