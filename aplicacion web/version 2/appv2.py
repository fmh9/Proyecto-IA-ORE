"""
==============================================================
  O.R.E. — Optimización del Rendimiento Estudiantil
  Aplicación Web (app.py) — v4  |  Modelo: m3_premium
==============================================================
  Novedades v4:
    · Nuevo dataset con variable género (gender_M)
    · AUC mejorado: 0.9223 | Accuracy: 83.85%
    · Campo "Género" añadido al formulario
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

COURSE_WEEKS = 34

# ─────────────────────────────────────────────
# CSS
# ─────────────────────────────────────────────
st.markdown("""
<style>
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
html, body, [class*="css"] { font-family: 'Inter', sans-serif; }

.stApp { background-color: #f4f6fb; color: #1a1a2e; }

.ore-header {
    background: linear-gradient(135deg, #1a1a2e 0%, #0f3460 100%);
    padding: 2.2rem 2.5rem; border-radius: 16px;
    margin-bottom: 1.8rem; box-shadow: 0 6px 24px rgba(0,0,0,0.15);
}
.ore-header h1   { color:#ffffff; font-size:2.6rem; font-weight:700; margin:0; }
.ore-header .sub { color:#90caf9; font-size:0.95rem; margin-top:0.2rem; letter-spacing:.08em; }
.ore-header p    { color:#cfd8f0; font-size:0.98rem; margin-top:0.8rem; line-height:1.6; max-width:700px; }

.result-pass {
    background:#e8f5e9; border-left:6px solid #2e7d32;
    padding:1.6rem 2rem; border-radius:14px; color:#1b5e20;
    box-shadow:0 2px 12px rgba(46,125,50,0.12);
}
.result-pass .rt { font-size:1.7rem; font-weight:700; margin:0 0 .3rem; color:#1b5e20; }
.result-pass .rp { font-size:1rem; color:#2e7d32; margin:0; }

.result-fail {
    background:#ffebee; border-left:6px solid #c62828;
    padding:1.6rem 2rem; border-radius:14px; color:#b71c1c;
    box-shadow:0 2px 12px rgba(198,40,40,0.12);
}
.result-fail .rt { font-size:1.7rem; font-weight:700; margin:0 0 .3rem; color:#b71c1c; }
.result-fail .rp { font-size:1rem; color:#c62828; margin:0; }

[data-testid="stSidebar"] { background-color:#ffffff; border-right:1px solid #e0e4ef; }
[data-testid="stSidebar"] label { color:#1a1a2e !important; font-weight:500; }

.stButton > button {
    width:100%; background:linear-gradient(135deg,#1565c0,#0d47a1);
    color:#ffffff !important; border:none; border-radius:10px;
    padding:.85rem; font-size:1.05rem; font-weight:600;
    letter-spacing:.02em; transition:all .2s; margin-top:.4rem;
}
.stButton > button:hover {
    background:linear-gradient(135deg,#1976d2,#1565c0);
    box-shadow:0 4px 14px rgba(21,101,192,.35); transform:translateY(-1px);
}

.welcome-card {
    background:#ffffff; border-radius:12px; padding:1.4rem 1.6rem;
    border:1px solid #e0e4ef; color:#1a1a2e;
    box-shadow:0 2px 8px rgba(0,0,0,.05);
}
.welcome-card h4 { color:#0d47a1; margin-top:0; }

footer { visibility:hidden; }
</style>
""", unsafe_allow_html=True)


# ─────────────────────────────────────────────
# MAPEOS
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

EDUCATION_NAMES = {
    "Bachillerato o equivalente":      "A Level or Equivalent",
    "Titulación universitaria":        "HE Qualification",
    "Educación secundaria o inferior": "Lower Than A Level",
    "Sin titulación formal":           "No Formal Quals",
    "Postgrado o máster":              "Post Graduate Qualification",
}

CREDITS_HELP = """
**¿Cuántos créditos representa cada opción?**

| Créditos | Equivalencia orientativa |
|----------|--------------------------|
| 30       | Media jornada — 1 asignatura ligera |
| 60       | Carga estándar — 2 asignaturas |
| 90       | Carga media-alta — 3 asignaturas |
| **120**  | **Curso completo normal (referencia)** |
| 150      | Carga elevada — puede generar estrés |
| 240+     | Carga muy alta — riesgo de sobrecarga lectiva |

Un curso completo estándar equivale a **120 créditos**.
Superar este umbral implica mayor exigencia y puede afectar al rendimiento.
"""


# ─────────────────────────────────────────────
# CARGA DEL MODELO
# ─────────────────────────────────────────────
@st.cache_resource
def load_model():
    path = "ore_model.pkl"
    if not os.path.exists(path):
        st.error("No se encontró `ore_model.pkl`. Ejecuta primero: `python train_model.py`")
        st.stop()
    artefacto = joblib.load(path)
    if isinstance(artefacto, dict):
        return artefacto["pipeline"], artefacto["feature_cols"]
    return artefacto, None

pipeline, FEATURE_COLS = load_model()


# ─────────────────────────────────────────────
# INGENIERÍA DE VARIABLES
# ─────────────────────────────────────────────
def engineer_features(total_clics: float, studied_credits: int, module_code: str) -> dict:
    log_c  = np.log(total_clics + 1)
    log_c2 = log_c ** 2
    extras = {
        "log_clics":                   log_c,
        "log_clics_sq":                log_c2,
        "log_clics_x_studied_credits": log_c * studied_credits,
    }
    for mod in ["BBB", "CCC", "DDD", "EEE", "FFF", "GGG"]:
        extras[f"log_clics_x_module_{mod}"] = log_c if module_code == mod else 0.0
    return extras


# ─────────────────────────────────────────────
# CONSTRUCCIÓN DEL VECTOR DE FEATURES
# ─────────────────────────────────────────────
def build_feature_vector(
    prev_attempts, studied_credits, weekly_clicks,
    module_code, education_es, imd_band, age_band,
    disability, gender_male,
) -> tuple[pd.DataFrame, int]:

    total_clics = weekly_clicks * COURSE_WEEKS
    row = {col: 0.0 for col in FEATURE_COLS}

    row["num_of_prev_attempts"] = prev_attempts
    row["studied_credits"]      = studied_credits
    row["disability_Y"]         = 1.0 if disability else 0.0
    row["gender_M"]             = 1.0 if gender_male else 0.0

    for mod in ["BBB", "CCC", "DDD", "EEE", "FFF", "GGG"]:
        row[f"code_module_{mod}"] = 1.0 if module_code == mod else 0.0

    row["code_presentation_2014J"] = 1.0

    education_en = EDUCATION_NAMES.get(education_es, education_es)
    edu_map = {
        "HE Qualification":            "highest_education_HE Qualification",
        "Lower Than A Level":          "highest_education_Lower Than A Level",
        "No Formal Quals":             "highest_education_No Formal quals",
        "Post Graduate Qualification": "highest_education_Post Graduate Qualification",
    }
    if education_en in edu_map:
        row[edu_map[education_en]] = 1.0

    imd_map = {
        "10-20%": "imd_band_10-20",   "20-30%": "imd_band_20-30%",
        "30-40%": "imd_band_30-40%",  "40-50%": "imd_band_40-50%",
        "50-60%": "imd_band_50-60%",  "60-70%": "imd_band_60-70%",
        "70-80%": "imd_band_70-80%",  "80-90%": "imd_band_80-90%",
        "90-100%": "imd_band_90-100%",
    }
    if imd_band in imd_map:
        row[imd_map[imd_band]] = 1.0

    if age_band == "35-55":
        row["age_band_35-55"] = 1.0
    elif age_band == "55+":
        row["age_band_55<="] = 1.0

    extras = engineer_features(total_clics, studied_credits, module_code)
    for k, v in extras.items():
        if k in row:
            row[k] = v

    return pd.DataFrame([row], columns=FEATURE_COLS), total_clics


# ─────────────────────────────────────────────
# MOTOR DE RECOMENDACIONES
# ─────────────────────────────────────────────
def generate_recommendations(
    prediction, prob_pass,
    prev_attempts, studied_credits, weekly_clicks, total_clics,
    education_es, imd_band, disability, student_name,
) -> list[dict]:
    recs = []
    name          = student_name.strip() if student_name.strip() else "el estudiante"
    overload      = studied_credits > 120
    low_clicks    = total_clics < 500
    medium_clicks = 500 <= total_clics < 1200
    high_attempts = prev_attempts >= 2
    low_imd       = imd_band in ["0-10%", "10-20%", "20-30%"]

    if overload:
        recs.append({"icon": "⚠️", "titulo": "Riesgo de sobrecarga lectiva",
            "texto": (
                f"{name} tiene matriculados {studied_credits} créditos, por encima del umbral "
                "estándar de un curso completo (120 créditos). Esta carga lectiva elevada puede "
                "generar estrés y menor rendimiento en cada módulo. Se recomienda valorar con el "
                "tutor si es posible redistribuir o reducir la carga.")})

    if low_clicks:
        recs.append({"icon": "💻", "titulo": "Aumentar actividad en la plataforma",
            "texto": (
                f"{name} registra muy poca actividad en el entorno virtual "
                f"({weekly_clicks} clics/semana · {total_clics:,} totales estimados). "
                "Se recomienda dedicar al menos 1 hora diaria a revisar materiales, "
                "participar en foros y completar los cuestionarios de práctica.")})
    elif medium_clicks:
        recs.append({"icon": "📈", "titulo": "Mejorar la interacción virtual",
            "texto": (
                f"El nivel de interacción de {name} con la plataforma es moderado "
                f"({weekly_clicks} clics/semana · {total_clics:,} totales estimados). "
                "Revisar los recursos adicionales (vídeos, lecturas opcionales) "
                "ayudará a consolidar los conceptos clave.")})
    else:
        recs.append({"icon": "🖥️", "titulo": "Buen uso de la plataforma",
            "texto": (
                f"{name} muestra una actividad digital alta "
                f"({weekly_clicks} clics/semana · {total_clics:,} totales estimados). "
                "Mantén ese ritmo y asegúrate de que los clics se traducen en comprensión real.")})

    if high_attempts:
        recs.append({"icon": "🔄", "titulo": "Refuerzo en áreas de dificultad",
            "texto": (
                f"Con {prev_attempts} intentos previos en el módulo, es clave que {name} "
                "identifique los bloques temáticos que generaron más dificultad y busque "
                "tutoría personalizada o grupos de estudio para reforzarlos.")})

    if education_es in ["Educación secundaria o inferior", "Sin titulación formal"]:
        recs.append({"icon": "🧱", "titulo": "Apoyo en formación previa",
            "texto": (
                f"El nivel de formación previa de {name} puede suponer un reto adicional. "
                "Se recomienda aprovechar los módulos de nivelación y el apoyo académico "
                "de la institución antes de afrontar los temas más avanzados.")})

    if low_imd:
        recs.append({"icon": "🤝", "titulo": "Recursos de apoyo socioeconómico",
            "texto": (
                f"El índice socioeconómico de {name} indica posibles barreras de acceso. "
                "Se recomienda informarse sobre becas, ayudas económicas y programas "
                "de soporte institucional disponibles.")})

    if disability:
        recs.append({"icon": "♿", "titulo": "Adaptaciones curriculares",
            "texto": (
                f"Es importante que {name} solicite las adaptaciones curriculares y tecnológicas "
                "a las que tiene derecho para garantizar las mismas oportunidades de éxito.")})

    if prediction == 1 and prob_pass >= 0.75:
        recs.append({"icon": "🌟", "titulo": "¡Sigue así!",
            "texto": (f"{name} va por muy buen camino. Mantener la constancia es clave. "
                      "Considera participar en proyectos de investigación o tutorías entre iguales.")})
    elif prediction == 1 and prob_pass < 0.75:
        recs.append({"icon": "⚡", "titulo": "Resultado positivo, pero con margen de mejora",
            "texto": (f"{name} está en zona positiva aunque con cierto riesgo. "
                      "Establece sesiones de estudio regulares con una agenda clara.")})
    elif prediction == 0 and prob_pass >= 0.35:
        recs.append({"icon": "🎯", "titulo": "Cerca del umbral — se puede revertir",
            "texto": (f"{name} está cerca del límite. Con un esfuerzo adicional sostenido "
                      "en las próximas semanas es posible revertir la situación.")})
    else:
        recs.append({"icon": "🚨", "titulo": "Riesgo elevado — acción inmediata recomendada",
            "texto": (f"El riesgo de que {name} no supere el módulo es elevado. "
                      "Se recomienda solicitar una tutoría urgente con el asesor académico.")})

    recs.append({"icon": "🧘", "titulo": "Bienestar y descanso",
        "texto": ("El descanso y la salud mental son igual de importantes que el estudio. "
                  "Dormir 7-9 horas y usar la técnica Pomodoro (25 min estudio / 5 min pausa) "
                  "mejora significativamente el rendimiento y la retención.")})

    return recs


# ══════════════════════════════════════════════════════
#  INTERFAZ
# ══════════════════════════════════════════════════════

st.markdown("""
<div class="ore-header">
  <h1>🎓 Proyecto O.R.E.</h1>
  <p class="sub">OPTIMIZACIÓN DEL RENDIMIENTO ESTUDIANTIL</p>
  <p>
    Introduce los datos del estudiante en el panel lateral y pulsa
    <strong>Predecir Resultado</strong>. El sistema analizará el perfil
    mediante el modelo <em>m3_premium</em> (Regresión Logística con
    ingeniería de variables · ROC-AUC&nbsp;0.92) entrenado sobre el
    <strong>Open University Learning Analytics Dataset (OULAD)</strong>.
  </p>
</div>
""", unsafe_allow_html=True)

# ── Sidebar ──────────────────────────────────
with st.sidebar:

    st.markdown("## 👤 Estudiante")
    student_name = st.text_input(
        "Nombre del estudiante",
        placeholder="Ej: María García",
        help="Introduce el nombre para personalizar las recomendaciones.",
    )
    gender_option = st.radio(
        "Género",
        options=["Mujer", "Hombre"],
        horizontal=True,
        help="Variable incluida en el modelo predictivo.",
    )
    gender_male = gender_option == "Hombre"
    st.markdown("---")

    st.markdown("## 📊 Rendimiento Académico")
    prev_attempts = st.number_input(
        "Intentos previos en este módulo",
        min_value=0, max_value=6, value=0, step=1,
        help="Número de veces que el estudiante ha cursado este módulo anteriormente.",
    )
    studied_credits = st.selectbox(
        "Créditos matriculados",
        options=[30, 60, 90, 120, 150, 240, 300, 360, 480, 655],
        index=3,
        help=CREDITS_HELP,
    )
    if studied_credits > 120:
        st.warning(
            f"⚠️ **Sobrecarga lectiva** — {studied_credits} créditos supera el umbral "
            "de un curso completo (120). Se tendrá en cuenta en las recomendaciones."
        )

    weekly_clicks = st.number_input(
        "Interacciones semanales en la plataforma virtual",
        min_value=0, max_value=1500, value=25, step=5,
        help=(
            "Introduce el promedio de interacciones **por semana** en el entorno virtual. "
            f"El sistema lo multiplicará por {COURSE_WEEKS} semanas automáticamente.\n\n"
            "**Referencia orientativa:**\n"
            "- Muy baja actividad: < 15 interacciones/semana\n"
            "- Actividad moderada: 15 – 35 interacciones/semana\n"
            "- Actividad alta: > 35 interacciones/semana"
        ),
    )
    total_preview = weekly_clicks * COURSE_WEEKS
    st.caption(f"📊 Total estimado del curso: **{total_preview:,} interacciones** ({COURSE_WEEKS} semanas)")
    st.markdown("---")

    st.markdown("## 🏫 Área de Estudio")
    module_label = st.selectbox("Módulo cursado", options=list(MODULE_NAMES.keys()))
    module_code  = MODULE_NAMES[module_label]
    st.markdown("---")

    st.markdown("## 🧑‍🎓 Perfil del Estudiante")
    education_es = st.selectbox(
        "Nivel de educación más alto",
        options=list(EDUCATION_NAMES.keys()),
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
    age_band   = st.selectbox("Rango de edad", options=["Under 35", "35-55", "55+"])
    disability = st.toggle("¿Tiene alguna discapacidad reconocida?", value=False)

    st.markdown("---")
    predict_btn = st.button("🔍 Predecir Resultado", use_container_width=True)

    st.markdown("<br>", unsafe_allow_html=True)
    st.markdown("""
    <div style="padding:1rem; background:#eef2ff; border-radius:10px;
                border:1px solid #c5cae9; font-size:0.82rem;
                color:#283593; line-height:2; text-align:center;">
        <strong>👨‍💻 Equipo de desarrollo</strong><br>
        Fernando Martínez · Aarón Sánchez<br>
        Julen Granell · Alfons Juan<br>
        Javier González
    </div>
    """, unsafe_allow_html=True)


# ── Resultados ───────────────────────────────
if predict_btn:

    display_name = student_name.strip() if student_name.strip() else "el estudiante"

    X_input, total_clics = build_feature_vector(
        prev_attempts=prev_attempts,
        studied_credits=studied_credits,
        weekly_clicks=weekly_clicks,
        module_code=module_code,
        education_es=education_es,
        imd_band=imd_band,
        age_band=age_band,
        disability=disability,
        gender_male=gender_male,
    )

    prediction    = pipeline.predict(X_input)[0]
    probabilities = pipeline.predict_proba(X_input)[0]
    prob_fail     = probabilities[0]
    prob_pass     = probabilities[1]

    col_res, col_prob = st.columns([3, 2], gap="large")

    with col_res:
        if prediction == 1:
            st.markdown(f"""
            <div class="result-pass">
                <p class="rt">✅ Predicción: APRUEBA</p>
                <p class="rp"><strong>{display_name.capitalize()}</strong> tiene una probabilidad
                estimada del <strong>{prob_pass*100:.1f}%</strong> de superar el módulo.</p>
            </div>""", unsafe_allow_html=True)
        else:
            st.markdown(f"""
            <div class="result-fail">
                <p class="rt">❌ Predicción: NO APRUEBA</p>
                <p class="rp"><strong>{display_name.capitalize()}</strong> tiene una probabilidad
                estimada del <strong>{prob_fail*100:.1f}%</strong> de no superar el módulo.</p>
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
                "Nombre", "Género", "Intentos previos", "Créditos matriculados",
                "Clics semanales (VLE)", "Clics totales estimados",
                "Módulo", "Nivel de educación",
                "Nivel socioeconómico", "Rango de edad", "Discapacidad",
            ],
            "Valor": [
                display_name, gender_option, prev_attempts, studied_credits,
                f"{weekly_clicks} / semana", f"{total_clics:,} ({COURSE_WEEKS} semanas)",
                module_label, education_es,
                imd_band, age_band, "Sí" if disability else "No",
            ],
        }
        st.dataframe(pd.DataFrame(perfil), use_container_width=True, hide_index=True)

    st.markdown("---")
    st.markdown("### 💡 Recomendaciones Personalizadas")
    st.caption(f"Plan de acción generado a partir del perfil de **{display_name}**.")

    recs = generate_recommendations(
        prediction=prediction, prob_pass=prob_pass,
        prev_attempts=prev_attempts, studied_credits=studied_credits,
        weekly_clicks=weekly_clicks, total_clics=total_clics,
        education_es=education_es, imd_band=imd_band,
        disability=disability, student_name=display_name,
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

# ── Bienvenida ───────────────────────────────
else:
    col1, col2, col3 = st.columns(3, gap="large")
    cards = [
        ("🧠", "Modelo m3_premium",
         "Regresión Logística con ingeniería de variables avanzada entrenada sobre "
         "<strong>31.000+ registros</strong> reales. ROC-AUC: <strong>0.92</strong>."),
        ("⚡", "Predicción Instantánea",
         "Introduce el perfil del estudiante en el panel lateral y obtén "
         "una estimación de su probabilidad de éxito en <strong>menos de 1 segundo</strong>."),
        ("🎯", "Consejos Accionables",
         "El sistema genera <strong>recomendaciones específicas y personalizadas</strong> "
         "incluyendo avisos de sobrecarga lectiva, actividad virtual y perfil socioeconómico."),
    ]
    for col, (icon, title, desc) in zip([col1, col2, col3], cards):
        with col:
            st.markdown(
                f'<div class="welcome-card"><h4>{icon} {title}</h4>'
                f'<p style="color:#444;font-size:.95rem;line-height:1.6;">{desc}</p></div>',
                unsafe_allow_html=True,
            )
    st.markdown("<br>", unsafe_allow_html=True)
    st.info(
        "👈 **Introduce el nombre y los datos del estudiante en el panel lateral** "
        "y pulsa **Predecir Resultado**."
    )
