"""
==============================================================
  O.R.E. — Optimización del Rendimiento Estudiantil
  Script de Entrenamiento del Modelo (train_model.py)
==============================================================
  Descripción:
    - Carga el dataset OULAD optimizado.
    - Prepara las features y el target (final_result).
    - Entrena un modelo de Regresión Logística.
    - Evalúa el modelo con métricas clave.
    - Guarda el modelo entrenado como 'ore_model.pkl'.

  Uso:
    python train_model.py
==============================================================
"""

import pandas as pd
import numpy as np
import joblib
import os

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.metrics import (
    classification_report,
    confusion_matrix,
    roc_auc_score,
    accuracy_score,
)

# ─────────────────────────────────────────────
# CONFIGURACIÓN
# ─────────────────────────────────────────────
_SCRIPT_DIR   = os.path.dirname(os.path.abspath(__file__))
DATASET_PATH  = os.path.join(_SCRIPT_DIR, "oulad_optimizado_final.csv")
MODEL_OUTPUT  = os.path.join(_SCRIPT_DIR, "ore_model.pkl")
TARGET_COL    = "final_result"
RANDOM_STATE  = 42
TEST_SIZE     = 0.2


def load_data(path: str) -> pd.DataFrame:
    """Carga el dataset desde un archivo CSV."""
    print(f"[INFO] Cargando dataset desde: {path}")
    df = pd.read_csv(path)
    print(f"[INFO] Dataset cargado: {df.shape[0]} filas × {df.shape[1]} columnas")
    return df


def prepare_features(df: pd.DataFrame, target: str):
    """
    Separa el DataFrame en features (X) y target (y).
    Todas las columnas excepto el target son features.
    """
    X = df.drop(columns=[target])
    y = df[target]
    print(f"[INFO] Features ({X.shape[1]}): {list(X.columns)}")
    print(f"[INFO] Distribución del target:\n{y.value_counts().to_string()}")
    return X, y


def build_pipeline() -> Pipeline:
    """
    Construye el pipeline de ML:
      1. StandardScaler  → normaliza las features numéricas.
      2. LogisticRegression → clasificador binario.
    Usar un Pipeline garantiza que el mismo scaler aplicado al train
    se reutilice en producción sin data leakage.
    """
    pipeline = Pipeline([
        ("scaler", StandardScaler()),
        ("classifier", LogisticRegression(
            max_iter=1000,
            random_state=RANDOM_STATE,
            class_weight="balanced",   # maneja el posible desbalance de clases
            solver="lbfgs",
            C=1.0,                     # regularización por defecto
        )),
    ])
    return pipeline


def evaluate_model(model: Pipeline, X_test: pd.DataFrame, y_test: pd.Series) -> None:
    """Imprime métricas de evaluación del modelo sobre el conjunto de test."""
    y_pred  = model.predict(X_test)
    y_proba = model.predict_proba(X_test)[:, 1]

    print("\n" + "=" * 55)
    print("  MÉTRICAS DE EVALUACIÓN DEL MODELO")
    print("=" * 55)
    print(f"  Accuracy  : {accuracy_score(y_test, y_pred):.4f}")
    print(f"  ROC-AUC   : {roc_auc_score(y_test, y_proba):.4f}")
    print("\n  Reporte de Clasificación:")
    print(classification_report(
        y_test, y_pred,
        target_names=["No Aprueba (0)", "Aprueba (1)"]
    ))
    print("  Matriz de Confusión:")
    cm = confusion_matrix(y_test, y_pred)
    print(f"    TN={cm[0,0]}  FP={cm[0,1]}")
    print(f"    FN={cm[1,0]}  TP={cm[1,1]}")
    print("=" * 55 + "\n")


def save_model(model: Pipeline, output_path: str) -> None:
    """Guarda el pipeline completo (scaler + modelo) con joblib."""
    joblib.dump(model, output_path)
    size_kb = os.path.getsize(output_path) / 1024
    print(f"[INFO] Modelo guardado en: '{output_path}' ({size_kb:.1f} KB)")


def main():
    print("\n" + "=" * 55)
    print("   O.R.E. — Entrenamiento del Modelo")
    print("=" * 55 + "\n")

    # 1. Cargar datos
    df = load_data(DATASET_PATH)

    # 2. Preparar features y target
    X, y = prepare_features(df, TARGET_COL)

    # 3. Dividir en train / test
    X_train, X_test, y_train, y_test = train_test_split(
        X, y,
        test_size=TEST_SIZE,
        random_state=RANDOM_STATE,
        stratify=y,            # mantiene la proporción de clases en ambos splits
    )
    print(f"\n[INFO] Train: {X_train.shape[0]} muestras | Test: {X_test.shape[0]} muestras")

    # 4. Construir y entrenar el pipeline
    pipeline = build_pipeline()
    print("[INFO] Entrenando modelo de Regresión Logística...")
    pipeline.fit(X_train, y_train)
    print("[INFO] Entrenamiento completado.")

    # 5. Evaluar el modelo
    evaluate_model(pipeline, X_test, y_test)

    # 6. Guardar el modelo
    save_model(pipeline, MODEL_OUTPUT)

    print("[✓] Proceso completado. Ya puedes lanzar la aplicación con:")
    print("      streamlit run app.py\n")


if __name__ == "__main__":
    main()
