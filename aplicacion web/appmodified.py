"""
==============================================================
  O.R.E. — Optimización del Rendimiento Estudiantil
  Aplicación Web (app.py) — v2
==============================================================
"""

import streamlit as st
import pandas as pd
import numpy as np
import joblib
import os

# ─────────────────────────────────────────────
# CONFIGURACIÓN DE PÁGINA
# ─────────────────────────────────────────────
st.set_page_config(
    page_title="O.R.E. · Rendimiento Estudiantil",
    page_icon="🎓",
    layout="wide",
    initial_sidebar_state="expanded",
)

# ─────────────────────────────────────────────
# CSS — Colores claros y completamente legibles
# ─────────────────────────────────────────────
st.markdown("""
<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');

html, body, [class*="css"] {
    font-family: 'Inter', sans-serif;
}

.stApp {
    background-color: #f4f6fb;
    color: #1a1a2e;
}

/* Cabecera */
.ore-header {
    background: linear-gradient(135deg, #1a1a2e 0%, #0f3460 100%);
    padding: 2.2rem 2.5rem;
    border-radius: 16px;
    margin-bottom: 1.8rem;
    box-shadow: 0 6px 24px rgba(0,0,0,0.15);
}
.ore-header h1 { color: #ffffff; font-size: 2.6rem; font-weight: 700; margin: 0; }
.ore-header .acronym { color: #90caf9; font-size: 0.95rem; margin-top: 0.2rem; letter-spacing: 0.08em; }
.ore-header p { color: #cfd8f0; font-size: 0.98rem; margin-top: 0.8rem; line-height: 1.6; max-width: 700px; }

/* Tarjeta APRUEBA */
.result-pass {
    background: #e8f5e9;
    border-left: 6px solid #2e7d32;
    padding: 1.6rem 2rem;
    border-radius: 14px;
    color: #1b5e20;
    box-shadow: 0 2px 12px rgba(46,125,50,0.12);
}
.result-pass .result-title { font-size: 1.7rem; font-weight: 700; margin: 0 0 0.3rem 0; color: #1b5e20; }
.result-pass .result-prob  { font-size: 1rem; color: #2e7d32; margin: 0; }

/* Tarjeta NO APRUEBA */
.result-fail {
    background: #ffebee;
    border-left: 6px solid #c62828;
    padding: 1.6rem 2rem;
    border-radius: 14px;
    color: #b71c1c;
    box-shadow: 0 2px 12px rgba(198,40,40,0.12);
}
.result-fail .result-title { font-size: 1.7rem; font-weight: 700; margin: 0 0 0.3rem 0; color: #b71c1c; }
.result-fail .result-prob  { font-size: 1rem; color: #c62828; margin: 0; }

/* Sidebar */
[data-testid="stSidebar"] {
    background-color: #ffffff;
    border-right: 1px solid #e0e4ef;
}
[data-testid="stSidebar"] label {
    color: #1a1a2e !important;
    font-weight: 500;
}

/* Botón principal */
.stButton > button {
    width: 100%;
    background: linear-gradient(135deg, #1565c0, #0d47a1);
    color: #ffffff !important;
    border: none;
    border-radius: 10px;
    padding: 0.85rem;
    font-size: 1.05rem;
    font-weight: 600;
    letter-spacing: 0.02em;
    transition: all 0.2s;
    margin-top: 0.4rem;
}
.stButton > button:hover {
    background: linear-gradient(135deg, #1976d2, #1565c0);
    box-shadow: 0 4px 14px rgba(21,101,192,0.35);
    transform: translateY(-1px);
}

/* Cards de bienvenida */
.welcome-card {
    background: #ffffff;
    border-radius: 12px;
    padding: 1.4rem 1.6rem;
    border: 1px solid #e0e4ef;
    color: #1a1a2e;
    box-shadow: 0 2px 8px rgba(0,0,0,0.05);
}
.welcome-card h4 { color: #0d47a1; margin-top: 0; }

footer { visibility: hidden; }
</style>
""", unsafe_allow_html=True)


# ─────────────────────────────────────────────
# MAPEO DE MÓDULOS (nombre legible -> código)
# ─────────────────────────────────────────────
MODULE_NAMES = {
    "Humanidades":                                        "AAA",
    "Ciencias Sociales (Psicología y comportamiento)":   "BBB",
    "Ciencias Exactas (Física y matemáticas puras)":     "CCC",
    "Ciencias Sociales (Gestión y diseño de proyectos)": "DDD",
    "Ingenierías":                                       "EEE",
    "Tecnología, Programación y Computación":            "FFF",
    "Salud":                                             "GGG",
}

# ─────────────────────────────────────────────
# CARGA DEL MODELO
# ─────────────────────────────────────────────
@st.cache_resource
def load_model():
    model_path = "ore_model.pkl"
    if not os.path.exists(model_path):
        st.error(
            "No se encontró `ore_model.pkl`. "
            "Ejecuta primero: `python train_model.py`"
        )
        st.stop()
    return joblib.load(model_path)

model = load_model()

FEATURE_COLUMNS = [
    "num_of_prev_attempts", "studied_credits", "total_clics",
    "code_module_BBB", "code_module_CCC", "code_module_DDD",
    "code_module_EEE", "code_module_FFF", "code_module_GGG",
    "code_presentation_2013J", "code_presentation_2014B", "code_presentation_2014J",
    "highest_education_HE Qualification", "highest_education_Lower Than A Level",
    "highest_education_No Formal quals", "highest_education_Post Graduate Qualification",
    "imd_band_10-20", "imd_band_20-30%", "imd_band_30-40%", "imd_band_40-50%",
    "imd_band_50-60%", "imd_band_60-70%", "imd_band_70-80%",
    "imd_band_80-90%", "imd_band_90-100%",
    "age_band_35-55", "age_band_55<=", "disability_Y",
]


# ─────────────────────────────────────────────
# CONSTRUCCIÓN DEL VECTOR DE FEATURES
# ─────────────────────────────────────────────
def build_feature_vector(
    prev_attempts, studied_credits, total_clicks,
    module_code, education, imd_band, age_band, disability,
) -> pd.DataFrame:
    row = {col: 0 for col in FEATURE_COLUMNS}

    row["num_of_prev_attempts"] = prev_attempts
    row["studied_credits"]      = studied_credits
    row["total_clics"]          = total_clicks

    module_map = {
        "BBB": "code_module_BBB", "CCC": "code_module_CCC",
        "DDD": "code_module_DDD", "EEE": "code_module_EEE",
        "FFF": "code_module_FFF", "GGG": "code_module_GGG",
    }
    if module_code in module_map:
        row[module_map[module_code]] = 1

    # Convocatoria fijada a 2014J (la más reciente del dataset)
    row["code_presentation_2014J"] = 1

    edu_map = {
        "HE Qualification":            "highest_education_HE Qualification",
        "Lower Than A Level":          "highest_education_Lower Than A Level",
        "No Formal Quals":             "highest_education_No Formal quals",
        "Post Graduate Qualification": "highest_education_Post Graduate Qualification",
    }
    if education in edu_map:
        row[edu_map[education]] = 1

    imd_map = {
        "10-20%": "imd_band_10-20",   "20-30%": "imd_band_20-30%",
        "30-40%": "imd_band_30-40%",  "40-50%": "imd_band_40-50%",
        "50-60%": "imd_band_50-60%",  "60-70%": "imd_band_60-70%",
        "70-80%": "imd_band_70-80%",  "80-90%": "imd_band_80-90%",
        "90-100%": "imd_band_90-100%",
    }
    if imd_band in imd_map:
        row[imd_map[imd_band]] = 1

    if age_band == "35-55":
        row["age_band_35-55"] = 1
    elif age_band == "55+":
        row["age_band_55<="] = 1

    row["disability_Y"] = 1 if disability else 0

    return pd.DataFrame([row], columns=FEATURE_COLUMNS)


# ─────────────────────────────────────────────
# MOTOR DE RECOMENDACIONES
# ─────────────────────────────────────────────
def generate_recommendations(
    prediction, probability_pass,
    prev_attempts, studied_credits, total_clicks,
    education, imd_band, disability, student_name,
) -> list[dict]:
    recs = []
    name = student_name.strip() if student_name.strip() else "el estudiante"

    low_clicks    = total_clicks < 500
    medium_clicks = 500 <= total_clicks < 1200
    high_attempts = prev_attempts >= 2
    low_imd       = imd_band in ["0-10%", "10-20%", "20-30%"]

    # --- Actividad en plataforma ---
    if low_clicks:
        recs.append({
            "icon": "💻",
            "titulo": "Aumentar actividad en la plataforma",
            "texto": (
                f"{name} registra muy poca actividad en el entorno virtual ({total_clicks} clics). "
                "Se recomienda dedicar al menos 1 hora diaria a revisar materiales, "
                "participar en foros y completar los cuestionarios de práctica."
            ),
        })
    elif medium_clicks:
        recs.append({
            "icon": "📈",
            "titulo": "Mejorar la interacción virtual",
            "texto": (
                f"El nivel de interacción de {name} con la plataforma es moderado. "
                "Revisar activamente los recursos adicionales (vídeos, lecturas opcionales) "
                "ayudará a consolidar los conceptos clave."
            ),
        })
    else:
        recs.append({
            "icon": "🖥️",
            "titulo": "Buen uso de la plataforma",
            "texto": (
                f"{name} muestra una actividad digital alta. "
                "Mantén ese ritmo y asegúrate de que los clics se traducen en comprensión real, "
                "no solo en revisión superficial del material."
            ),
        })

    # --- Intentos previos ---
    if high_attempts:
        recs.append({
            "icon": "🔄",
            "titulo": "Refuerzo en áreas de dificultad",
            "texto": (
                f"Con {prev_attempts} intentos previos en el módulo, es clave que {name} identifique "
                "exactamente qué bloques temáticos generaron más dificultad y busque tutoría "
                "personalizada o grupos de estudio para reforzarlos."
            ),
        })

    # --- Nivel educativo ---
    if education in ["Lower Than A Level", "No Formal Quals"]:
        recs.append({
            "icon": "🧱",
            "titulo": "Apoyo en formación previa",
            "texto": (
                f"El nivel de formación previa de {name} puede suponer un reto adicional. "
                "Se recomienda aprovechar los módulos de nivelación y el apoyo académico "
                "que ofrece la institución antes de afrontar los temas más avanzados."
            ),
        })

    # --- Nivel socioeconómico ---
    if low_imd:
        recs.append({
            "icon": "🤝",
            "titulo": "Recursos de apoyo socioeconómico",
            "texto": (
                f"El índice socioeconómico de {name} indica posibles barreras de acceso a recursos. "
                "Se recomienda informarse sobre becas, ayudas económicas y programas "
                "de soporte institucional disponibles."
            ),
        })

    # --- Discapacidad ---
    if disability:
        recs.append({
            "icon": "♿",
            "titulo": "Adaptaciones curriculares",
            "texto": (
                f"Es importante que {name} solicite las adaptaciones curriculares y tecnológicas "
                "a las que tiene derecho. El servicio de atención a la discapacidad de la "
                "institución puede garantizar las mismas oportunidades de éxito."
            ),
        })

    # --- Resultado de la predicción ---
    if prediction == 1 and probability_pass >= 0.75:
        recs.append({
            "icon": "🌟",
            "titulo": "¡Sigue así!",
            "texto": (
                f"{name} va por muy buen camino. Mantener la constancia y la disciplina "
                "es clave. Considera participar en proyectos de investigación o tutorías "
                "entre iguales para seguir creciendo académicamente."
            ),
        })
    elif prediction == 1 and probability_pass < 0.75:
        recs.append({
            "icon": "⚡",
            "titulo": "Resultado positivo, pero con margen de mejora",
            "texto": (
                f"{name} está en zona positiva, aunque con cierto riesgo. "
                "Conviene revisar los temas menos seguros y establecer sesiones "
                "de estudio regulares con una agenda clara y verificable."
            ),
        })
    elif prediction == 0 and probability_pass >= 0.35:
        recs.append({
            "icon": "🎯",
            "titulo": "Cerca del umbral — se puede revertir",
            "texto": (
                f"{name} está cerca del límite. Con un esfuerzo adicional y sostenido "
                "en las próximas semanas es posible revertir la situación. "
                "Crear un plan de estudio semanal detallado y cumplirlo es el primer paso."
            ),
        })
    else:
        recs.append({
            "icon": "🚨",
            "titulo": "Riesgo elevado — acción inmediata recomendada",
            "texto": (
                f"El riesgo de que {name} no supere el módulo es elevado. "
                "Se recomienda solicitar una tutoría urgente con el asesor académico "
                "para diseñar un plan de recuperación a medida lo antes posible."
            ),
        })

    # --- Bienestar universal ---
    recs.append({
        "icon": "🧘",
        "titulo": "Bienestar y descanso",
        "texto": (
            "El descanso y la salud mental son igual de importantes que las horas de estudio. "
            "Dormir entre 7 y 9 horas y usar la técnica Pomodoro (25 min estudio / 5 min pausa) "
            "mejora significativamente el rendimiento y la retención."
        ),
    })

    return recs


# ══════════════════════════════════════════════════════════
#  INTERFAZ
# ══════════════════════════════════════════════════════════

st.markdown("""
<div class="ore-header">
  <h1>🎓 Proyecto O.R.E.</h1>
  <p class="acronym">OPTIMIZACIÓN DEL RENDIMIENTO ESTUDIANTIL</p>
  <p>
    Introduce los datos del estudiante en el panel lateral y pulsa <strong>Predecir Resultado</strong>.
    El sistema analizará el perfil mediante un modelo de <em>Regresión Logística</em> entrenado sobre
    el <strong>Open University Learning Analytics Dataset (OULAD)</strong> y generará una predicción
    junto con recomendaciones personalizadas.
  </p>
</div>
""", unsafe_allow_html=True)

# ── Sidebar ────────────────────────────────────
with st.sidebar:

    st.markdown("## 👤 Estudiante")
    student_name = st.text_input(
        "Nombre del estudiante",
        placeholder="Ej: María García",
        help="Introduce tu nombre para una mejor personalización de las recomendaciones.",
    )
    st.markdown("---")

    st.markdown("## 📊 Rendimiento Académico")
    prev_attempts = st.number_input(
        "Intentos previos en este módulo",
        min_value=0, max_value=6, value=0, step=1,
    )
    studied_credits = st.selectbox(
        "Créditos matriculados",
        options=[30, 60, 90, 120, 150],
        index=1,
    )
    total_clicks = st.number_input(
        "Número de interacciones semanales con la plataforma virtual",
        min_value=0, max_value=30000, value=800, step=50,
        help="Número de interacciones semanales con el entorno virtual de aprendizaje.",
    )
    st.markdown("---")

    st.markdown("## 🏫 Área de Estudio")
    module_label = st.selectbox(
        "Módulo cursado",
        options=list(MODULE_NAMES.keys()),
    )
    module_code = MODULE_NAMES[module_label]
    st.markdown("---")

    st.markdown("## 🧑‍🎓 Perfil del Estudiante")
    education = st.selectbox(
        "Nivel de educación más alto",
        options=[
            "Sin estudios formales",
            "ESO",
            "Formación Profesional",
            "Bachillerato o equivalente",
            "Postgrado",
        ],
    )
    imd_band = st.selectbox(
        "Nivel socioeconómico (IMD)",
        options=[
            "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
            "50-60%", "60-70%", "70-80%", "80-90%", "90-100%",
        ],
        index=4,
        help="0-10% = mayor privación · 90-100% = menor privación.",
    )
    age_band = st.selectbox(
        "Rango de edad",
        options=["Menor a 35", "Entre 35 y 55", "55 o más"],
    )
    disability = st.toggle("¿Tiene alguna discapacidad reconocida?", value=False)

    st.markdown("---")
    predict_btn = st.button("🔍 Predecir Resultado", use_container_width=True)

    # Créditos del equipo
    st.markdown("<br>", unsafe_allow_html=True)
    st.markdown("""
    <div style="padding: 1rem; background:#eef2ff; border-radius: 10px;
                border: 1px solid #c5cae9; font-size: 0.82rem;
                color: #283593; line-height: 2; text-align: center;">
        <strong>👨‍💻 Equipo de desarrollo</strong><br>
        Fernando Martínez · Aarón Sánchez<br>
        Julen Granell · Alfons Juan<br>
        Javier González
    </div>
    """, unsafe_allow_html=True)


# ── Resultados ────────────────────────────────
if predict_btn:

    display_name = student_name.strip() if student_name.strip() else "el estudiante"

    X_input = build_feature_vector(
        prev_attempts=prev_attempts,
        studied_credits=studied_credits,
        total_clicks=total_clicks,
        module_code=module_code,
        education=education,
        imd_band=imd_band,
        age_band=age_band,
        disability=disability,
    )

    prediction    = model.predict(X_input)[0]
    probabilities = model.predict_proba(X_input)[0]
    prob_fail     = probabilities[0]
    prob_pass     = probabilities[1]

    col_res, col_prob = st.columns([3, 2], gap="large")

    with col_res:
        if prediction == 1:
            st.markdown(f"""
            <div class="result-pass">
                <p class="result-title">✅ Predicción: APRUEBA</p>
                <p class="result-prob">
                    <strong>{display_name.capitalize()}</strong> tiene una probabilidad estimada
                    del <strong>{prob_pass*100:.1f}%</strong> de superar el módulo.
                </p>
            </div>""", unsafe_allow_html=True)
        else:
            st.markdown(f"""
            <div class="result-fail">
                <p class="result-title">❌ Predicción: NO APRUEBA</p>
                <p class="result-prob">
                    <strong>{display_name.capitalize()}</strong> tiene una probabilidad estimada
                    del <strong>{prob_fail*100:.1f}%</strong> de no superar el módulo.
                </p>
            </div>""", unsafe_allow_html=True)

    with col_prob:
        st.markdown("#### 📊 Probabilidades")
        st.metric("Probabilidad de Aprobar",   f"{prob_pass*100:.1f}%")
        st.metric("Probabilidad de Suspender", f"{prob_fail*100:.1f}%")
        st.progress(float(prob_pass), text="Nivel de éxito estimado")

    st.markdown("<br>", unsafe_allow_html=True)

    with st.expander("🗂️ Ver perfil completo analizado", expanded=False):
        perfil = {
            "Parámetro": [
                "Nombre", "Intentos previos", "Créditos matriculados",
                "Clics en VLE", "Módulo", "Nivel de educación",
                "Nivel socioeconómico", "Rango de edad", "Discapacidad",
            ],
            "Valor": [
                display_name, prev_attempts, studied_credits,
                total_clicks, module_label, education,
                imd_band, age_band, "Sí" if disability else "No",
            ],
        }
        st.dataframe(pd.DataFrame(perfil), use_container_width=True, hide_index=True)

    # ── Recomendaciones con componentes nativos Streamlit ──
    st.markdown("---")
    st.markdown("### 💡 Recomendaciones Personalizadas")
    st.caption(
        f"Plan de acción generado a partir del perfil de **{display_name}**."
    )

    recs = generate_recommendations(
        prediction=prediction,
        probability_pass=prob_pass,
        prev_attempts=prev_attempts,
        studied_credits=studied_credits,
        total_clicks=total_clicks,
        education=education,
        imd_band=imd_band,
        disability=disability,
        student_name=display_name,
    )

    for rec in recs:
        with st.container(border=True):
            st.markdown(f"**{rec['icon']} {rec['titulo']}**")
            st.write(rec["texto"])

    st.markdown("<br>", unsafe_allow_html=True)
    st.caption(
        "⚠️ Esta predicción es orientativa y se basa en patrones estadísticos del dataset OULAD. "
        "No sustituye la evaluación de un tutor académico."
    )

# ── Bienvenida ────────────────────────────────
else:
    col1, col2, col3 = st.columns(3, gap="large")
    cards = [
        ("🧠", "Modelo de ML",
         "Regresión Logística entrenada con más de <strong>31.000 registros</strong> reales "
         "del Open University Learning Analytics Dataset (OULAD)."),
        ("⚡", "Predicción Instantánea",
         "Introduce el perfil del estudiante en el panel lateral y obtén "
         "una estimación de su probabilidad de éxito en <strong>menos de 1 segundo</strong>."),
        ("🎯", "Consejos Accionables",
         "El sistema genera <strong>recomendaciones específicas y personalizadas</strong> "
         "adaptadas a las características individuales de cada estudiante."),
    ]
    for col, (icon, title, desc) in zip([col1, col2, col3], cards):
        with col:
            st.markdown(
                f'<div class="welcome-card"><h4>{icon} {title}</h4>'
                f'<p style="color:#444;font-size:0.95rem;line-height:1.6;">{desc}</p></div>',
                unsafe_allow_html=True,
            )

    st.markdown("<br>", unsafe_allow_html=True)
    st.info(
        "👈 **Introduce el nombre y los datos del estudiante en el panel lateral** "
        "y pulsa **Predecir Resultado**."
    )
