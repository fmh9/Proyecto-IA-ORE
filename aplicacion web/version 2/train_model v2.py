"""
==============================================================
  O.R.E. — Optimización del Rendimiento Estudiantil
  Script de Entrenamiento — Modelo m3_premium (train_model.py)
==============================================================
  Dataset : oulad_listo_para_regresion2.csv
  Novedad : incluye variable gender_M → AUC 0.9223

  Ingeniería de variables (modelo R m3_premium):
    · log_clics  = log(total_clics + 1)
    · log_clics² = log_clics²
    · log_clics × studied_credits
    · log_clics × code_module_{BBB..GGG}

  Uso:  python train_model.py
==============================================================
"""

import pandas as pd
import numpy as np
import joblib
import os
import warnings
warnings.filterwarnings("ignore")

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.metrics import (
    classification_report, confusion_matrix,
    roc_auc_score, accuracy_score,
)

# ─────────────────────────────────────────────
# CONFIGURACIÓN
# ─────────────────────────────────────────────
DATASET_PATH = "oulad_listo_para_regresion2.csv"
MODEL_OUTPUT  = "ore_model.pkl"
TARGET_COL    = "final_result"
RANDOM_STATE  = 42
TEST_SIZE     = 0.20
COURSE_WEEKS  = 34


def load_data(path: str) -> pd.DataFrame:
    print(f"[INFO] Cargando dataset: {path}")
    df = pd.read_csv(path)
    print(f"[INFO] {df.shape[0]} filas × {df.shape[1]} columnas")
    return df


def engineer_features(df: pd.DataFrame) -> pd.DataFrame:
    """
    Ingeniería de variables del modelo R m3_premium.
    total_clics se sustituye por su forma logarítmica.
    """
    df = df.copy()
    df["log_clics"]    = np.log(df["total_clics"] + 1)
    df["log_clics_sq"] = df["log_clics"] ** 2
    df["log_clics_x_studied_credits"] = df["log_clics"] * df["studied_credits"]
    for mod in ["BBB", "CCC", "DDD", "EEE", "FFF", "GGG"]:
        df[f"log_clics_x_module_{mod}"] = df["log_clics"] * df[f"code_module_{mod}"]
    df = df.drop(columns=["total_clics"])
    return df


def build_pipeline() -> Pipeline:
    return Pipeline([
        ("scaler", StandardScaler()),
        ("clf", LogisticRegression(
            max_iter=2000,
            random_state=RANDOM_STATE,
            class_weight="balanced",
            solver="lbfgs",
            C=1.0,
        )),
    ])


def evaluate_model(model, X_test, y_test) -> None:
    y_pred  = model.predict(X_test)
    y_proba = model.predict_proba(X_test)[:, 1]
    print("\n" + "=" * 58)
    print("  MÉTRICAS — Modelo m3_premium (nuevo dataset)")
    print("=" * 58)
    print(f"  Accuracy  : {accuracy_score(y_test, y_pred):.4f}")
    print(f"  ROC-AUC   : {roc_auc_score(y_test, y_proba):.4f}")
    print("\n  Reporte de Clasificación:")
    print(classification_report(
        y_test, y_pred,
        target_names=["No Aprueba (0)", "Aprueba (1)"]
    ))
    cm = confusion_matrix(y_test, y_pred)
    print("  Matriz de Confusión:")
    print(f"    TN={cm[0,0]}  FP={cm[0,1]}")
    print(f"    FN={cm[1,0]}  TP={cm[1,1]}")
    print("=" * 58 + "\n")


def main():
    print("\n" + "=" * 58)
    print("   O.R.E. · Entrenamiento modelo m3_premium")
    print("=" * 58 + "\n")

    df = load_data(DATASET_PATH)
    df = engineer_features(df)

    feature_cols = [c for c in df.columns if c != TARGET_COL]
    X = df[feature_cols]
    y = df[TARGET_COL]

    print(f"[INFO] Features totales: {len(feature_cols)}")
    print(f"[INFO] Distribución target:\n{y.value_counts().to_string()}\n")

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=TEST_SIZE, random_state=RANDOM_STATE, stratify=y
    )
    print(f"[INFO] Train: {X_train.shape[0]} | Test: {X_test.shape[0]}")

    pipe = build_pipeline()
    print("[INFO] Entrenando...")
    pipe.fit(X_train, y_train)
    print("[INFO] Entrenamiento completado.\n")

    evaluate_model(pipe, X_test, y_test)

    artefacto = {
        "pipeline":     pipe,
        "feature_cols": feature_cols,
        "course_weeks": COURSE_WEEKS,
    }
    joblib.dump(artefacto, MODEL_OUTPUT)
    size_kb = os.path.getsize(MODEL_OUTPUT) / 1024
    print(f"[✓] Modelo guardado: '{MODEL_OUTPUT}' ({size_kb:.1f} KB)")
    print("[✓] Listo. Ejecuta:  streamlit run app.py\n")


if __name__ == "__main__":
    main()
