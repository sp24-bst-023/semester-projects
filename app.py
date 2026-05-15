import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from streamlit_lottie import st_lottie
import requests
from scipy import stats
from statsmodels.tsa.holtwinters import ExponentialSmoothing
from statsmodels.tsa.seasonal import seasonal_decompose
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.ensemble import IsolationForest
import io
import time
import datetime
import random


# ==============================================================================
# SECTION 1: SYSTEM ASSETS & LOTTIE PIPELINE
# ==============================================================================


@st.cache_resource
def load_lottie_assets(url: str):
    """Securely fetches high-fidelity animations for the Command Center UI."""
    try:
        response = requests.get(url, timeout=20)
        return response.json() if response.status_code == 200 else None
    except Exception:
        return None


lottie_main_hero = load_lottie_assets("https://assets10.lottiefiles.com/packages/lf20_6p8ve6.json")
lottie_analytics = load_lottie_assets("https://assets1.lottiefiles.com/packages/lf20_qpwb7t6c.json")


# ==============================================================================
# SECTION 2: MAX-CONTRAST UI (BRIGHT PILLARS + BUTTONS)
# ==============================================================================


st.set_page_config(
    page_title="Zenith Ultra | Muneeb",
    layout="wide",
    initial_sidebar_state="expanded"
)


def apply_ui_overrides():
    """Forces extreme contrast for sidebar, pillars and buttons."""
    st.markdown("""
    <style>
        .stApp { 
            background: #f1f5f9;
        }

        section[data-testid="stSidebar"] { 
            background-color: #FFFFFF !important;
            color: #020617 !important;
            min-width: 420px !important;
            border-right: 5px solid #6366F1;
        }
        section[data-testid="stSidebar"] * {
            color: #020617 !important;
        }

        .streamlit-expanderHeader { 
            background-color: #FFFFFF !important;
            color: #020617 !important;
            font-size: 22px !important;
            font-weight: 900 !important;
            border: 3px solid #6366F1 !important;
            border-radius: 14px !important;
            opacity: 1 !important;
            padding: 16px 18px !important;
            margin-bottom: 12px !important;
            box-shadow: 0 8px 20px rgba(15, 23, 42, 0.18) !important;
            display: flex !important;
            align-items: center !important;
        }
        .streamlit-expanderHeader div,
        .streamlit-expanderHeader span,
        .streamlit-expanderHeader p {
            color: #020617 !important;
            font-weight: 950 !important;
            opacity: 1 !important;
        }

        .streamlit-expanderContent {
            background-color: #F9FAFB !important;
            color: #020617 !important;
            border-radius: 0 0 14px 14px !important;
        }

        .streamlit-expanderHeader:hover {
            background-color: #EEF2FF !important;
            color: #111827 !important;
            border-color: #4F46E5 !important;
            transform: scale(1.02);
            box-shadow: 0 12px 28px rgba(79, 70, 229, 0.40) !important;
        }
        .streamlit-expanderHeader:hover div,
        .streamlit-expanderHeader:hover span,
        .streamlit-expanderHeader:hover p {
            color: #111827 !important;
        }

        section[data-testid="stSidebar"] div.stButton > button[kind="secondary"] {
            background-color: #FFFFFF !important;
            color: #0F172A !important;
            border: 2px solid #E5E7EB !important;
            border-radius: 16px !important;
            padding: 14px 18px !important;
            font-weight: 800 !important;
            text-align: left !important;
            width: 100% !important;
            box-shadow: 0 6px 14px rgba(15, 23, 42, 0.10);
        }
        section[data-testid="stSidebar"] div.stButton > button[kind="secondary"] div {
            color: #0F172A !important;
        }
        section[data-testid="stSidebar"] div.stButton > button[kind="secondary"]:hover {
            background-color: #EEF2FF !important;
            color: #111827 !important;
            border-color: #6366F1 !important;
            transform: translateX(8px);
            box-shadow: 0 10px 22px rgba(99, 102, 241, 0.35);
        }
        section[data-testid="stSidebar"] div.stButton > button[kind="secondary"]:hover div {
            color: #111827 !important;
        }

        div[data-testid="stMetric"] {
            background: #FFFFFF !important;
            padding: 20px !important;
            border-radius: 20px !important;
            box-shadow: 0 8px 18px rgba(15, 23, 42, 0.12) !important;
            border: 1px solid #E2E8F0 !important;
            transition: all 0.25s ease-out !important;
        }
        div[data-testid="stMetric"]:hover {
            transform: translateY(-10px) !important;
            box-shadow: 0 18px 40px rgba(99, 102, 241, 0.25) !important;
            border-color: #6366F1 !important;
        }
        [data-testid="stMetricLabel"] { 
            font-size: 0.95rem !important; 
            color: #475569 !important; 
            font-weight: 700 !important; 
        }
        [data-testid="stMetricValue"] { 
            font-size: 1.8rem !important; 
            color: #0F172A !important; 
            font-weight: 900 !important; 
        }
    </style>
    """, unsafe_allow_html=True)


apply_ui_overrides()


# ==============================================================================
# SECTION 3: UNIVERSAL DATA ENGINE (SMART AUTO-MAPPER)
# ==============================================================================


def initialize_mainframe():
    """Initializes the data engine with session persistence."""
    if 'df' not in st.session_state:
        np.random.seed(99)
        size = 2000
        st.session_state.df = pd.DataFrame({
            "Index_Time": pd.date_range("2024-01-01", periods=size, freq='H'),
            "Metric_A": np.random.randint(50000, 120000, size),
            "Metric_B": np.random.randint(10000, 40000, size),
            "Rate_1": np.random.uniform(0.01, 0.20, size),
            "Score_1": np.random.normal(90, 4, size),
            "Category_1": np.random.choice(['Group A', 'Group B', 'Group C', 'Group D'], size),
            "Category_2": np.random.choice(['Type 1', 'Type 2', 'Type 3'], size),
            "Source": np.random.choice(['Channel 1', 'Channel 2', 'Channel 3'], size),
            "Latency_ms": np.random.uniform(20, 150, size)
        })
    if 'active_page' not in st.session_state:
        st.session_state.active_page = "Executive Snapshot"


initialize_mainframe()
df_master = st.session_state.df

num_cols = df_master.select_dtypes(include=np.number).columns.tolist()
cat_cols = df_master.select_dtypes(exclude=np.number).columns.tolist()

primary_metric = num_cols[0] if num_cols else None
secondary_metric = num_cols[1] if len(num_cols) > 1 else primary_metric
primary_cat = cat_cols[0] if cat_cols else None
time_like_col = next(
    (c for c in df_master.columns if any(k in c.lower() for k in ["date", "time", "year", "index"])),
    df_master.columns[0]
)


# ==============================================================================
# SECTION 4: COMMAND SIDEBAR (5 CORE PILLARS + QUICK SEARCH)
# ==============================================================================


with st.sidebar:
    st.markdown("<h1 style='text-align:center; color:#6366F1; font-size:45px;'>ZENITH</h1>", unsafe_allow_html=True)
    if lottie_main_hero:
        st_lottie(lottie_main_hero, height=160)

    st.markdown("---")

    quick_query = st.text_input("🔍 Quick search (any column)", value="")
    if quick_query:
        mask = df_master.apply(
            lambda col: col.astype(str).str.contains(quick_query, case=False, na=False)
        )
        f_df = df_master[mask.any(axis=1)]
    else:
        f_df = df_master

    st.caption(f"{len(f_df):,} rows matched current search.")

    st.markdown("---")

    with st.expander("🚀 1. DATA FOUNDRY", expanded=True):
        if st.button("📡 Acquisition Hub"):
            st.session_state.active_page = "Acquisition"
        if st.button("🧼 Integrity Sentinel"):
            st.session_state.active_page = "Integrity"
        if st.button("🕵️ Outlier Detector"):
            st.session_state.active_page = "Outliers"
        if st.button("🔨 Feature Forge"):
            st.session_state.active_page = "Forge"
        if st.button("📖 Data Dictionary"):
            st.session_state.active_page = "Dictionary"

    with st.expander("🧪 2. INFERENCE ENGINE"):
        if st.button("📊 Stats Matrix"):
            st.session_state.active_page = "Stats"
        if st.button("🧪 Hypothesis Lab"):
            st.session_state.active_page = "Hypothesis"
        if st.button("📉 Correlation Heatmap"):
            st.session_state.active_page = "Correlation"
        if st.button("📡 ANOVA Variance"):
            st.session_state.active_page = "ANOVA"
        if st.button("🔢 Dist Fitting"):
            st.session_state.active_page = "Distribution"

    with st.expander("🧠 3. NEURAL HORIZONS"):
        if st.button("🔮 AI Forecast Model"):
            st.session_state.active_page = "Forecast"
        if st.button("🤖 Behavioral Clusters"):
            st.session_state.active_page = "Clusters"
        if st.button("⚡ Growth Simulator"):
            st.session_state.active_page = "Simulator"
        if st.button("🧬 PCA Dimensionality"):
            st.session_state.active_page = "PCA"
        if st.button("📈 Trend Decomp"):
            st.session_state.active_page = "Trend"

    with st.expander("🎨 4. VISUAL INTELLIGENCE"):
        if st.button("👑 Executive Snapshot"):
            st.session_state.active_page = "Executive Snapshot"
        if st.button("🎨 Relationship Map"):
            st.session_state.active_page = "Relationship"
        if st.button("🌳 Sunburst Hierarchy"):
            st.session_state.active_page = "Sunburst"
        if st.button("🎻 Violin Analysis"):
            st.session_state.active_page = "Violin"

    with st.expander("🛠️ 5. OPERATIONAL"):
        if st.button("👤 Professional CV"):
            st.session_state.active_page = "Portfolio"
        if st.button("📄 Enterprise Export"):
            st.session_state.active_page = "Export"
        if st.button("📚 Knowledge Base"):
            st.session_state.active_page = "Knowledge"

    st.markdown("---")
    st.info(f"System Health: Optimized | Admin: Muneeb")


# ==============================================================================
# SECTION 5: GENERIC KPI LAYER
# ==============================================================================


st.markdown("### 💠 Universal Dataset Benchmarks")

k1, k2, k3, k4, k5 = st.columns(5)

if primary_metric:
    k1.metric("Primary Metric Sum", f"{f_df[primary_metric].sum():,.2f}")
    k2.metric("Primary Metric Mean", f"{f_df[primary_metric].mean():,.2f}")
else:
    k1.metric("Primary Metric Sum", "N/A")
    k2.metric("Primary Metric Mean", "N/A")

k3.metric("Rows", f"{len(f_df):,}")
k4.metric("Numeric Columns", f"{len(num_cols)}")
k5.metric("Categorical Columns", f"{len(cat_cols)}")
st.divider()


# ==============================================================================
# SECTION 6: MODULE LOGIC
# ==============================================================================


def select_numeric(label, default=None):
    if not num_cols:
        st.warning("No numeric columns detected in this dataset.")
        return None
    return st.selectbox(label, num_cols, index=max(num_cols.index(default), 0) if default in num_cols else 0)


def select_categorical(label, default=None):
    if not cat_cols:
        st.warning("No categorical columns detected in this dataset.")
        return None
    return st.selectbox(label, cat_cols, index=max(cat_cols.index(default), 0) if default in cat_cols else 0)


# ---------------- EXECUTIVE SNAPSHOT (MINIMAL) ----------------
if st.session_state.active_page == "Executive Snapshot":
    st.title("👑 Executive Snapshot")

    time_col = st.selectbox(
        "Time / Index column",
        df_master.columns,
        index=df_master.columns.get_loc(time_like_col)
    )
    metric_col = select_numeric("Primary numeric metric", default=primary_metric)
    cat_for_rank = select_categorical("Categorical for ranking", default=primary_cat)

    c1, c2 = st.columns(2)

    with c1:
        if metric_col:
            df_trend = f_df.sort_values(by=time_col).copy()
            df_trend = df_trend[[time_col, metric_col]].dropna()
            if not df_trend.empty:
                fig_line = px.line(
                    df_trend,
                    x=time_col,
                    y=metric_col,
                    title=f"{metric_col} over {time_col}",
                    color_discrete_sequence=["#6366F1"]
                )
                fig_line.update_layout(
                    xaxis_title=time_col,
                    yaxis_title=metric_col,
                    margin=dict(l=40, r=20, t=60, b=40)
                )
                st.plotly_chart(fig_line, use_container_width=True)
            else:
                st.info("No non-missing data available for the selected metric/time combination.")

    with c2:
        if metric_col and cat_for_rank:
            df_rank = f_df[[cat_for_rank, metric_col]].dropna()
            if not df_rank.empty:
                agg_df = df_rank.groupby(cat_for_rank)[metric_col].sum().sort_values(ascending=False)
                top_n = min(5, len(agg_df))
                agg_top = agg_df.head(top_n).reset_index()
                agg_top.columns = [cat_for_rank, metric_col]

                fig_rank = px.bar(
                    agg_top,
                    x=metric_col,
                    y=cat_for_rank,
                    orientation="h",
                    title=f"Top {top_n} {cat_for_rank} by {metric_col} (sum)",
                    color=metric_col,
                    color_continuous_scale="Blues"
                )
                fig_rank.update_layout(
                    xaxis_title=metric_col,
                    yaxis_title=cat_for_rank,
                    margin=dict(l=80, r=20, t=60, b=40),
                    coloraxis_showscale=False
                )
                st.plotly_chart(fig_rank, use_container_width=True)
            else:
                st.info("No non-missing data available for the selected metric/category.")


# ---------------- ACQUISITION HUB ----------------
elif st.session_state.active_page == "Acquisition":
    st.title("📡 Data Acquisition Hub")
    uploaded = st.file_uploader("Upload CSV dataset", type="csv")
    if uploaded:
        st.session_state.df = pd.read_csv(uploaded)
        st.success("Dataset loaded successfully. Universal engine remapped.")
        st.rerun()
    st.dataframe(f_df.head(500), use_container_width=True)


# ---------------- INTEGRITY SENTINEL ----------------
elif st.session_state.active_page == "Integrity":
    st.title("🧼 Integrity Sentinel")

    st.subheader("Scan Configuration")

    col_opts1, col_opts2 = st.columns(2)
    with col_opts1:
        opt_missing_summary = st.checkbox("Missing-value summary (totals)", value=True)
        opt_missing_table = st.checkbox("Per-column missing table", value=True)
    with col_opts2:
        opt_missing_bar = st.checkbox("Per-column missing bar chart", value=True)
        opt_dup_summary = st.checkbox("Duplicate-row summary", value=True)

    st.markdown("---")
    run_scan = st.button("🚀 Run Integrity Scan")

    if run_scan:
        total_rows = len(f_df)
        if opt_missing_summary or opt_missing_table or opt_missing_bar:
            null_counts = f_df.isnull().sum()
            null_pct = (null_counts / total_rows * 100).round(2) if total_rows > 0 else 0
            missing_df = pd.DataFrame({
                "Column": f_df.columns,
                "Missing_Count": null_counts.values,
                "Missing_%": null_pct.values
            }).sort_values("Missing_%", ascending=False)

        if opt_missing_summary:
            st.subheader("Missing-value overview")
            st.write(f"Total missing values in dataset: **{int(null_counts.sum())}**")

        if opt_missing_table:
            st.subheader("Per-column missing-value table")
            st.dataframe(missing_df, use_container_width=True)

        if opt_missing_bar:
            st.subheader("Per-column missing-value bar chart")
            st.bar_chart(missing_df.set_index("Column")["Missing_Count"])

        if opt_dup_summary:
            st.subheader("Duplicate-row summary")
            dup_count = int(f_df.duplicated().sum())
            st.write(f"Total duplicate rows: **{dup_count}**")
            if dup_count > 0:
                st.caption("Duplicates are rows that repeat across all columns.")

    st.markdown("---")
    st.subheader("Deep Cleanse Operations")

    c_clean1, c_clean2 = st.columns(2)
    with c_clean1:
        apply_drop_na = st.checkbox("Drop rows with any missing values", value=False)
    with c_clean2:
        apply_drop_dups = st.checkbox("Drop duplicate rows", value=True)

    st.markdown("##### Before vs After (Preview)")
    prev_rows = len(f_df)
    df_preview = f_df.copy()
    if apply_drop_na:
        df_preview = df_preview.dropna()
    if apply_drop_dups:
        df_preview = df_preview.drop_duplicates()
    new_rows = len(df_preview)
    removed_rows = prev_rows - new_rows

    c_prev, c_new, c_removed = st.columns(3)
    c_prev.metric("Current rows", f"{prev_rows:,}")
    c_new.metric("Rows after cleanse (preview)", f"{new_rows:,}")
    c_removed.metric("Rows that would be removed", f"{removed_rows:,}")

    if st.button("🧽 Apply Deep Cleanse"):
        st.session_state.df = df_preview
        st.success(
            f"Cleanse applied. Rows: {prev_rows:,} ➝ {new_rows:,} "
            f"(removed {removed_rows:,})."
        )
        st.caption("Preview of cleaned dataset (top 200 rows).")
        st.dataframe(df_preview.head(200), use_container_width=True)
        st.stop()


# ---------------- OUTLIERS (ENHANCED) ----------------
elif st.session_state.active_page == "Outliers":
    st.title("🕵️ Advanced Outlier Detector")

    target = select_numeric("Analyze numeric column", default=primary_metric)
    if target:
        x_choices = ["Row Index"] + [c for c in num_cols if c != target]
        x_choice = st.selectbox("X-axis for scatter", x_choices)

        iso_model = IsolationForest(contamination=0.05, random_state=42)
        preds = iso_model.fit_predict(f_df[[target]].fillna(0))
        f_df_out = f_df.copy()
        f_df_out["__outlier_flag__"] = np.where(preds == -1, "Outlier", "Normal")

        st.metric("Detected anomalies", int((preds == -1).sum()))

        st.plotly_chart(
            px.box(f_df_out, y=target, points="all", title=f"Distribution & outliers: {target}"),
            use_container_width=True
        )

        if x_choice == "Row Index":
            f_df_out["__x__"] = np.arange(len(f_df_out))
            x_col_for_scatter = "__x__"
            x_title = "Row index"
        else:
            x_col_for_scatter = x_choice
            x_title = x_choice

        fig_scatter = px.scatter(
            f_df_out,
            x=x_col_for_scatter,
            y=target,
            color="__outlier_flag__",
            title=f"{target} vs {x_title} (Outliers highlighted)",
            color_discrete_map={"Normal": "#4B5563", "Outlier": "#EF4444"}
        )
        st.plotly_chart(fig_scatter, use_container_width=True)

        st.subheader("Outlier rows (top 50)")
        outlier_rows = f_df_out[f_df_out["__outlier_flag__"] == "Outlier"].drop(columns=["__outlier_flag__", "__x__"], errors="ignore")
        if outlier_rows.empty:
            st.info("No outliers detected with current settings.")
        else:
            st.dataframe(outlier_rows.head(50), use_container_width=True)


# ---------------- FEATURE FORGE (NEW) ----------------
elif st.session_state.active_page == "Forge":
    st.title("🔨 Feature Forge – Signal Lab")

    st.write("Design lightweight engineered features that are safe and generic for any dataset.")

    if not num_cols and not cat_cols:
        st.warning("No columns available for feature engineering.")
    else:
        st.markdown("#### Numeric feature transforms")
        if num_cols:
            base_num = st.selectbox("Base numeric column", num_cols, key="ff_base_num")
            op = st.selectbox(
                "Transformation",
                ["Z-score standardization", "Log transform (log1p)", "Square root", "Square"],
                key="ff_num_op"
            )

            if st.button("Create numeric feature"):
                col = f_df[base_num].astype(float)
                if op == "Z-score standardization":
                    new_series = (col - col.mean()) / (col.std() or 1)
                    suffix = "z"
                elif op == "Log transform (log1p)":
                    new_series = np.log1p(col.clip(lower=0))
                    suffix = "log1p"
                elif op == "Square root":
                    new_series = np.sqrt(col.clip(lower=0))
                    suffix = "sqrt"
                else:
                    new_series = col ** 2
                    suffix = "sq"

                new_name = f"{base_num}_{suffix}"
                st.session_state.df[new_name] = new_series
                st.success(f"Feature '{new_name}' created and added to dataset.")
                st.dataframe(
                    st.session_state.df[[base_num, new_name]].head(20),
                    use_container_width=True
                )

        st.markdown("---")
        st.markdown("#### Categorical frequency encoding")
        if cat_cols:
            cat_col = st.selectbox("Categorical column for frequency encoding", cat_cols, key="ff_cat_col")
            max_uniques = st.slider("Max uniques to encode (avoid huge cardinality)", 10, 500, 50)
            if st.button("Create frequency-encoded feature"):
                vc = f_df[cat_col].value_counts(normalize=True)
                if len(vc) > max_uniques:
                    vc = vc.head(max_uniques)
                freq_map = vc.to_dict()
                new_name = f"{cat_col}_freq"
                st.session_state.df[new_name] = f_df[cat_col].map(freq_map).fillna(0.0)
                st.success(f"Feature '{new_name}' created using normalized category frequencies.")
                st.dataframe(
                    st.session_state.df[[cat_col, new_name]].head(20),
                    use_container_width=True
                )


# ---------------- DATA DICTIONARY (NEW) ----------------
elif st.session_state.active_page == "Dictionary":
    st.title("📖 Data Dictionary – Schema & Roles")

    df = f_df
    total_rows = len(df)

    roles = []
    logical_types = []
    missing_counts = df.isnull().sum()
    unique_counts = df.nunique(dropna=True)

    for col in df.columns:
        dtype = df[col].dtype
        nunique = unique_counts[col]

        if pd.api.types.is_numeric_dtype(dtype):
            logical = "Numeric"
        elif pd.api.types.is_datetime64_any_dtype(dtype):
            logical = "Datetime"
        elif nunique > 0 and nunique == total_rows:
            logical = "Identifier"
        else:
            logical = "Categorical/Text"

        logical_types.append(logical)

        if logical == "Identifier":
            role = "ID / Key"
        elif logical == "Datetime":
            role = "Time"
        elif logical == "Numeric":
            role = "Measure"
        else:
            role = "Dimension"
        roles.append(role)

    def card_band(n):
        if n <= 10:
            return "Low"
        elif n <= 100:
            return "Medium"
        else:
            return "High"

    cardinality_band = [card_band(unique_counts[c]) for c in df.columns]
    is_constant = [unique_counts[c] == 1 for c in df.columns]

    numeric_min = []
    numeric_max = []
    for col in df.columns:
        if pd.api.types.is_numeric_dtype(df[col].dtype):
            numeric_min.append(df[col].min())
            numeric_max.append(df[col].max())
        else:
            numeric_min.append(None)
            numeric_max.append(None)

    examples = []
    for col in df.columns:
        example = df[col].dropna().iloc[0] if df[col].dropna().shape[0] > 0 else None
        examples.append(example)

    dict_df = pd.DataFrame({
        "Column": df.columns,
        "Role": roles,
        "Logical_Type": logical_types,
        "Pandas_Dtype": df.dtypes.astype(str).values,
        "Missing_Count": missing_counts.values,
        "Missing_%": (missing_counts / total_rows * 100).round(2) if total_rows > 0 else 0,
        "Unique_Count": unique_counts.values,
        "Cardinality_Band": cardinality_band,
        "Is_Constant": is_constant,
        "Min": numeric_min,
        "Max": numeric_max,
        "Example_Value": examples
    })

    st.dataframe(dict_df, use_container_width=True)

    st.markdown("---")
    c1, c2 = st.columns(2)
    with c1:
        st.markdown("#### Missing % by column")
        st.bar_chart(dict_df.set_index("Column")["Missing_%"])
    with c2:
        st.markdown("#### Unique counts (capped)")
        capped = dict_df.copy()
        capped["Unique_Count_Capped"] = capped["Unique_Count"].clip(upper=200)
        st.bar_chart(capped.set_index("Column")["Unique_Count_Capped"])


# ---------------- STATS MATRIX ----------------
elif st.session_state.active_page == "Stats":
    st.title("📊 Descriptive Statistics Matrix")
    if num_cols:
        st.dataframe(f_df[num_cols].describe().T, use_container_width=True)
    else:
        st.info("No numeric columns available for descriptive statistics.")


# ---------------- HYPOTHESIS LAB ----------------
elif st.session_state.active_page == "Hypothesis":
    st.title("🧪 Statistical Hypothesis Lab")
    target = select_numeric("Numeric column for one-sample t-test", default=primary_metric)
    if target:
        h0 = st.number_input("Null hypothesis mean", value=float(f_df[target].mean()))
        stat_v, p_v = stats.ttest_1samp(f_df[target].dropna(), h0)
        st.metric("P-Value", f"{p_v:.6f}")
        if p_v < 0.05:
            st.error("Reject null hypothesis (significant difference).")
        else:
            st.success("Fail to reject null hypothesis (no significant difference).")


# ---------------- CORRELATION HEATMAP (NEW) ----------------
elif st.session_state.active_page == "Correlation":
    st.title("📉 Correlation Heatmap")

    if len(num_cols) < 2:
        st.info("Need at least 2 numeric columns to compute correlations.")
    else:
        method = st.selectbox("Correlation method", ["pearson", "spearman", "kendall"], index=0)
        corr = f_df[num_cols].corr(method=method)

        st.subheader("Correlation matrix")
        fig_heat = px.imshow(
            corr,
            text_auto=".2f",
            color_continuous_scale="RdBu_r",
            origin="lower",
            aspect="auto",
            title=f"{method.title()} correlations among numeric features"
        )
        fig_heat.update_layout(margin=dict(l=40, r=40, t=60, b=40))
        st.plotly_chart(fig_heat, use_container_width=True)

        focus_col = st.selectbox("Focus on correlations with", num_cols)
        st.subheader(f"Correlations with {focus_col}")
        focus_series = corr[focus_col].drop(labels=[focus_col])
        focus_df = focus_series.sort_values(ascending=False).reset_index()
        focus_df.columns = ["Feature", "Correlation"]
        st.dataframe(focus_df, use_container_width=True)

        st.plotly_chart(
            px.bar(
                focus_df,
                x="Feature",
                y="Correlation",
                title=f"{method.title()} correlation with {focus_col}",
                color="Correlation",
                color_continuous_scale="RdBu_r",
                range_color=[-1, 1]
            ),
            use_container_width=True
        )


# ---------------- ANOVA ----------------
elif st.session_state.active_page == "ANOVA":
    st.title("📡 ANOVA Variance Engine")
    y = select_numeric("Dependent numeric variable", default=primary_metric)
    x = select_categorical("Categorical factor")
    if y and x and f_df[x].nunique() > 1:
        groups = [f_df[f_df[x] == g][y].dropna().values for g in f_df[x].dropna().unique()]
        if len(groups) >= 2:
            f_stat, p_a = stats.f_oneway(*groups)
            st.metric("ANOVA F-statistic", f"{f_stat:.4f}")
            st.metric("P-value", f"{p_a:.6f}")
            st.plotly_chart(px.box(f_df, x=x, y=y, color=x, title=f"{y} by {x}"))
        else:
            st.warning("Need at least two groups for ANOVA.")


# ---------------- DIST FITTING (NEW) ----------------
elif st.session_state.active_page == "Distribution":
    st.title("🔢 Distribution Fitting Lab")

    target = select_numeric("Numeric column to analyze", default=primary_metric)
    if target:
        data = f_df[target].dropna()
        if len(data) < 3:
            st.warning("Need at least 3 non-missing observations for distribution analysis.")
        else:
            st.subheader("Histogram with density & fitted normal curve")

            fig = px.histogram(
                data,
                x=target,
                nbins=40,
                histnorm="probability density",
                opacity=0.6,
                marginal="box",
                title=f"Distribution of {target}"
            )

            kde_x = np.linspace(data.min(), data.max(), 200)
            try:
                from scipy.stats import gaussian_kde
                kde = gaussian_kde(data)
                kde_y = kde(kde_x)
                fig.add_trace(
                    go.Scatter(
                        x=kde_x,
                        y=kde_y,
                        mode="lines",
                        name="KDE",
                        line=dict(color="#6366F1", width=2)
                    )
                )
            except Exception:
                pass

            mu, sigma = data.mean(), data.std(ddof=0)
            normal_y = stats.norm.pdf(kde_x, mu, sigma)
            fig.add_trace(
                go.Scatter(
                    x=kde_x,
                    y=normal_y,
                    mode="lines",
                    name="Normal fit",
                    line=dict(color="#EF4444", dash="dash")
                )
            )

            st.plotly_chart(fig, use_container_width=True)

            st.subheader("Shape statistics")
            skew = stats.skew(data)
            kurt = stats.kurtosis(data, fisher=True)
            c1, c2, c3, c4 = st.columns(4)
            c1.metric("Mean", f"{mu:.4f}")
            c2.metric("Std dev", f"{sigma:.4f}")
            c3.metric("Skewness", f"{skew:.4f}")
            c4.metric("Kurtosis", f"{kurt:.4f}")

            st.subheader("Normality test (Shapiro–Wilk)")
            try:
                sh_stat, sh_p = stats.shapiro(data)
                st.write(f"Statistic: **{sh_stat:.4f}**, p-value: **{sh_p:.6f}**")
                if sh_p < 0.05:
                    st.error("Data significantly deviates from normality (p < 0.05).")
                else:
                    st.success("Cannot reject normality (p ≥ 0.05).")
            except Exception as e:
                st.warning(f"Normality test not available: {e}")


# ---------------- FORECAST ----------------
elif st.session_state.active_page == "Forecast":
    st.title("🔮 AI Forecast Model")
    metric_col = select_numeric("Numeric series to forecast", default=primary_metric)
    if metric_col:
        try:
            series = f_df[metric_col].dropna()
            if len(series) < 20:
                st.warning("Need at least 20 observations for a stable forecast.")
            else:
                m = ExponentialSmoothing(series.tail(500), trend="add").fit()
                p = m.forecast(36)
                fig = go.Figure()
                fig.add_trace(go.Scatter(y=series.tail(100), name="History"))
                fig.add_trace(go.Scatter(y=p, name="Forecast", line=dict(dash='dash', color='red')))
                st.plotly_chart(fig, use_container_width=True)
        except Exception as e:
            st.error(f"Forecasting error: {e}")


# ---------------- BEHAVIORAL CLUSTERS ----------------
elif st.session_state.active_page == "Clusters":
    st.title("🤖 Behavioral Clusters")

    st.write("Unsupervised clustering to identify behavioral groups in your data.")

    if len(num_cols) < 2:
        st.warning("Need at least 2 numeric columns to perform clustering.")
    else:
        k = st.slider("Number of clusters (K)", 2, 10, 3)

        col_for_cluster = st.multiselect(
            "Numeric columns for clustering",
            num_cols,
            default=num_cols[:min(3, len(num_cols))]
        )

        if col_for_cluster and st.button("🔍 Run K-Means Clustering"):
            df_cluster = f_df[col_for_cluster].dropna()

            if len(df_cluster) < k:
                st.error(f"Not enough rows ({len(df_cluster)}) for {k} clusters.")
            else:
                scaler = StandardScaler()
                X_scaled = scaler.fit_transform(df_cluster)

                kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)
                clusters = kmeans.fit_predict(X_scaled)

                df_cluster = df_cluster.copy()
                df_cluster["Cluster"] = clusters

                c1, c2 = st.columns(2)

                with c1:
                    st.subheader("Cluster distribution")
                    cluster_counts = pd.Series(clusters).value_counts().sort_index()
                    st.bar_chart(cluster_counts)

                with c2:
                    st.subheader("Cluster sizes")
                    st.dataframe(
                        cluster_counts.reset_index().rename(
                            columns={"index": "Cluster", 0: "Count"}
                        ),
                        use_container_width=True
                    )

                if len(col_for_cluster) >= 2:
                    x_col = col_for_cluster[0]
                    y_col = col_for_cluster[1]

                    fig_scatter = px.scatter(
                        df_cluster,
                        x=x_col,
                        y=y_col,
                        color="Cluster",
                        title=f"Cluster visualization: {x_col} vs {y_col}",
                    )
                    st.plotly_chart(fig_scatter, use_container_width=True)

                st.subheader("Cluster characteristics (mean values)")
                cluster_summary = df_cluster.groupby("Cluster")[col_for_cluster].mean()
                st.dataframe(cluster_summary, use_container_width=True)


# ---------------- GROWTH SIMULATOR ----------------
elif st.session_state.active_page == "Simulator":
    st.title("⚡ Growth Simulator")

    st.write("Project future growth scenarios with configurable parameters.")

    metric_col = select_numeric("Base metric for simulation", default=primary_metric)

    if metric_col:
        df_sim = f_df[metric_col].dropna()
        current_value = df_sim.iloc[-1] if len(df_sim) > 0 else 0
        baseline_mean = df_sim.mean()
        baseline_std = df_sim.std()

        st.subheader("Simulation Parameters")

        col_param1, col_param2, col_param3 = st.columns(3)

        with col_param1:
            growth_rate = st.slider(
                "Monthly growth rate (%)",
                -20.0, 50.0, 5.0,
                step=0.5
            )

        with col_param2:
            volatility = st.slider(
                "Volatility (std % of mean)",
                0.0, 50.0, 10.0,
                step=1.0
            )

        with col_param3:
            months = st.slider(
                "Forecast months",
                1, 36, 12
            )

        if st.button("🚀 Run Simulation"):
            np.random.seed(42)

            simulated_values = [current_value]

            for month in range(months):
                growth_factor = 1 + (growth_rate / 100)
                noise = np.random.normal(0, baseline_std * volatility / 100)
                next_value = simulated_values[-1] * growth_factor + noise
                next_value = max(0, next_value)
                simulated_values.append(next_value)

            sim_df = pd.DataFrame({
                "Month": range(months + 1),
                "Projected_Value": simulated_values
            })

            st.subheader("Simulation Results")

            m1, m2, m3, m4 = st.columns(4)
            m1.metric("Starting value", f"{current_value:,.2f}")
            m2.metric("Ending projection", f"{simulated_values[-1]:,.2f}")
            if current_value != 0:
                total_growth = (simulated_values[-1] / current_value - 1) * 100
            else:
                total_growth = 0.0
            m3.metric("Total growth", f"{total_growth:,.1f}%")
            m4.metric("Simulation horizon", f"{months} months")

            fig_sim = px.line(
                sim_df,
                x="Month",
                y="Projected_Value",
                title=f"Growth Simulation: {metric_col} (rate: {growth_rate}%, vol: {volatility}%)",
                markers=True,
                color_discrete_sequence=['#10B981']
            )
            fig_sim.add_hline(
                y=baseline_mean,
                line_dash="dash",
                line_color="gray",
                annotation_text="Historical mean",
                annotation_position="right"
            )
            st.plotly_chart(fig_sim, use_container_width=True)

            st.subheader("Projection table")
            st.dataframe(sim_df.round(2), use_container_width=True)


# ---------------- TREND DECOMPOSITION ----------------
elif st.session_state.active_page == "Trend":
    st.title("📈 Trend Decomposition")

    st.write("Break down your time series into trend, seasonal, and residual components.")

    metric_col = select_numeric("Time series metric", default=primary_metric)

    if metric_col:
        df_trend = f_df.sort_values(by=time_like_col)
        series_data = df_trend[metric_col].dropna()

        if len(series_data) < 12:
            st.warning("Need at least 12 observations for meaningful decomposition.")
        else:
            st.subheader("Decomposition Configuration")

            col_dec1, col_dec2 = st.columns(2)

            with col_dec1:
                seasonal_period = st.slider(
                    "Seasonal period (observations per cycle)",
                    4, min(100, len(series_data) // 3),
                    12
                )

            with col_dec2:
                model_type = st.selectbox(
                    "Decomposition model",
                    ["additive", "multiplicative"],
                    index=0
                )

            if st.button("🔍 Decompose Series"):
                try:
                    decomposition = seasonal_decompose(
                        series_data,
                        model=model_type,
                        period=seasonal_period
                    )

                    decomp_df = pd.DataFrame({
                        "Original": decomposition.observed,
                        "Trend": decomposition.trend,
                        "Seasonal": decomposition.seasonal,
                        "Residual": decomposition.resid
                    })

                    st.subheader("Decomposition Components")

                    fig_decomp = make_subplots(
                        rows=4, cols=1,
                        subplot_titles=("Original", "Trend", "Seasonal", "Residual"),
                        shared_xaxes=True,
                        vertical_spacing=0.08
                    )

                    colors = ["#6366F1", "#10B981", "#F59E0B", "#EF4444"]

                    for i, (col_name, color) in enumerate(zip(decomp_df.columns, colors), 1):
                        fig_decomp.add_trace(
                            go.Scatter(
                                x=decomp_df.index,
                                y=decomp_df[col_name],
                                name=col_name,
                                line=dict(color=color, width=2)
                            ),
                            row=i, col=1
                        )

                    fig_decomp.update_yaxes(title_text="Value", row=1, col=1)
                    fig_decomp.update_yaxes(title_text="Trend", row=2, col=1)
                    fig_decomp.update_yaxes(title_text="Seasonal", row=3, col=1)
                    fig_decomp.update_yaxes(title_text="Residual", row=4, col=1)

                    fig_decomp.update_layout(height=900, title_text=f"Decomposition of {metric_col}")
                    st.plotly_chart(fig_decomp, use_container_width=True)

                    st.subheader("Component Statistics")

                    stats_decomp = pd.DataFrame({
                        "Component": decomp_df.columns,
                        "Mean": decomp_df.mean().values,
                        "Std Dev": decomp_df.std().values,
                        "Min": decomp_df.min().values,
                        "Max": decomp_df.max().values
                    })
                    st.dataframe(stats_decomp, use_container_width=True)

                except Exception as e:
                    st.error(f"Decomposition failed: {e}")


# ---------------- PCA (3D) ----------------
elif st.session_state.active_page == "PCA":
    st.title("🧬 PCA Dimensionality Engine")

    if len(num_cols) >= 3:
        df_numeric = f_df[num_cols].dropna().copy()

        if df_numeric.shape[0] < 3:
            st.warning("Need at least 3 complete rows for 3D PCA.")
        else:
            scaler = StandardScaler()
            X = scaler.fit_transform(df_numeric)

            pca = PCA(n_components=3)
            pcs = pca.fit_transform(X)

            p_df = pd.DataFrame(
                pcs,
                columns=["PC1", "PC2", "PC3"]
            )

            if cat_cols:
                df_color = f_df.loc[df_numeric.index, cat_cols[0]].reset_index(drop=True)
                p_df["Color"] = df_color
                color_arg = "Color"
            else:
                color_arg = None

            fig = px.scatter_3d(
                p_df,
                x="PC1",
                y="PC2",
                z="PC3",
                color=color_arg,
                title="3D Principal Component Projection",
            )
            st.plotly_chart(fig, use_container_width=True)

            evr = pca.explained_variance_ratio_
            c1, c2, c3 = st.columns(3)
            c1.metric("PC1 variance", f"{evr[0]*100:.1f}%")
            c2.metric("PC2 variance", f"{evr[1]*100:.1f}%")
            c3.metric("PC3 variance", f"{evr[2]*100:.1f}%")
    else:
        st.warning("Need at least 3 numeric columns for PCA.")


# ---------------- RELATIONSHIP MAP (CLEAN SCATTER, FIXED) ----------------
elif st.session_state.active_page == "Relationship":
    st.title("🎨 Relationship Map – Clean Scatter")

    if len(num_cols) < 2:
        st.info("Need at least 2 numeric columns to show relationships.")
    else:
        x_col = st.selectbox("X-axis numeric", num_cols, index=0)
        y_candidates = [c for c in num_cols if c != x_col]
        if not y_candidates:
            st.info("Select another numeric column for Y.")
        else:
            y_col = st.selectbox("Y-axis numeric", y_candidates, index=0)

            color_opt = st.selectbox("Color by (optional category)", ["None"] + cat_cols)

            cols = [x_col, y_col]
            if color_opt != "None":
                cols.append(color_opt)

            df_rel = f_df[cols].dropna()

            if df_rel.empty:
                st.info("No non-missing data for the selected combination.")
            else:
                fig_rel = px.scatter(
                    df_rel,
                    x=x_col,
                    y=y_col,
                    color=color_opt if color_opt != "None" else None,
                    opacity=0.65,
                    marginal_x="histogram",
                    marginal_y="histogram",
                    title=f"{y_col} vs {x_col}" + (f" colored by {color_opt}" if color_opt != "None" else "")
                )
                fig_rel.update_layout(
                    xaxis_title=x_col,
                    yaxis_title=y_col,
                    plot_bgcolor="#F9FAFB",
                    paper_bgcolor="#F9FAFB",
                    margin=dict(l=40, r=30, t=70, b=40),
                    legend_title_text=color_opt if color_opt != "None" else ""
                )
                st.plotly_chart(fig_rel, use_container_width=True)


# ---------------- CATEGORICAL SUMMARY (REPLACES SUNBURST) ----------------
elif st.session_state.active_page == "Sunburst":
    st.title("🌳 Categorical Summary")

    if not cat_cols:
        st.info("No categorical columns detected in this dataset.")
    else:
        cat_col = st.selectbox("Select categorical column", cat_cols, index=0)

        df_cat = f_df[[cat_col]].dropna()
        if df_cat.empty:
            st.info("No data for the selected column.")
        else:
            counts = df_cat[cat_col].value_counts().reset_index()
            counts.columns = [cat_col, "Count"]

            top_n = st.slider("Top N categories to display", 3, 20, 10)
            counts_top = counts.head(top_n)

            c1, c2 = st.columns(2)

            with c1:
                st.subheader("Bar view")
                fig_bar = px.bar(
                    counts_top,
                    x=cat_col,
                    y="Count",
                    text="Count",
                    title=f"Top {top_n} {cat_col} by frequency"
                )
                fig_bar.update_traces(textposition="outside")
                fig_bar.update_layout(
                    xaxis_title=cat_col,
                    yaxis_title="Count",
                    margin=dict(l=40, r=20, t=60, b=80)
                )
                st.plotly_chart(fig_bar, use_container_width=True)

            with c2:
                st.subheader("Donut view")
                fig_pie = px.pie(
                    counts_top,
                    names=cat_col,
                    values="Count",
                    hole=0.5,
                    title=f"Share of top {top_n} {cat_col}"
                )
                fig_pie.update_layout(margin=dict(l=20, r=20, t=60, b=20))
                st.plotly_chart(fig_pie, use_container_width=True)

            st.subheader("Category table")
            st.dataframe(counts_top, use_container_width=True)


# ---------------- VIOLIN (CLASSIC, TOP 6) ----------------
elif st.session_state.active_page == "Violin":
    st.title("🎻 Violin Analysis")

    y = select_numeric("Numeric variable (distribution)", default=primary_metric)
    x = select_categorical("Grouping factor (categories)", default=primary_cat)

    if y and x:
        df_v = f_df[[x, y]].dropna()

        top_cats = df_v[x].value_counts().head(6).index
        df_v = df_v[df_v[x].isin(top_cats)]

        fig_violin = px.violin(
            df_v,
            x=x,
            y=y,
            box=True,
            points=False,
            color=x,
            color_discrete_sequence=px.colors.qualitative.Set2,
            title=f"{y} distribution by {x}"
        )
        fig_violin.update_layout(
            xaxis_title=x,
            yaxis_title=y,
            showlegend=False,
            margin=dict(l=40, r=20, t=60, b=40)
        )
        st.plotly_chart(fig_violin, use_container_width=True)
    else:
        st.info("Select one numeric and one categorical column to draw violins.")


# ---------------- PORTFOLIO (WITH PHOTO + SKILLS) ----------------
elif st.session_state.active_page == "Portfolio":
    st.title("👤 Professional Profile – Muneeb ur Rehman")

    col_photo, col_text = st.columns([1, 2])

    with col_photo:
        # profile.jpg should be in a `www` folder next to this app.py
        st.image("www/profile.jpg", caption="Muneeb ur Rehman", width=220)

    with col_text:
        st.markdown("### **Muneeb ur Rehman**")
        st.markdown(
            "B.S. Statistics (Specialization in Data Science) – COMSATS University Islamabad"
        )
        st.markdown(
            "Aspiring data professional with a strong foundation in statistical thinking "
            "and hands‑on experience in data analytics and visualization using R, Python, and Power BI."
        )

        st.markdown("#### Education")
        st.markdown(
            "- B.S. in Statistics (Data Science specialization), COMSATS University Islamabad – in progress.\n"
            "- Schooling and college from Army Public School and College."
        )

        st.markdown("#### Key Skills")
        st.markdown(
            "- Data analytics and exploratory data analysis.\n"
            "- Data visualization and dashboarding in Power BI.\n"
            "- Statistical analysis and modeling in R.\n"
            "- Data manipulation and scripting in Python."
        )


# ---------------- EXPORT ----------------
elif st.session_state.active_page == "Export":
    st.title("📄 Enterprise Export")
    csv_data = f_df.to_csv(index=False).encode('utf-8')
    st.download_button("Download Filtered Dataset (.CSV)", csv_data, "Zenith_Universal_Export.csv")


# ---------------- KNOWLEDGE BASE (placeholder) ----------------
elif st.session_state.active_page == "Knowledge":
    st.title("📚 Knowledge Base")
    st.info("Knowledge base content coming soon.")


# ==============================================================================
# ARCHITECTURE LOGS
# ==============================================================================
# [LOG] Feature Forge: safe numeric transforms + frequency encoding for categorical columns.
# [LOG] Data Dictionary: schema, roles, cardinality, and examples for every column.
# [LOG] Inference Engine: Correlation Heatmap and Distribution Fitting Lab added.
# [LOG] Neural Horizons: Forecast, Behavioral Clusters, Growth Simulator, PCA (3D), Trend Decomposition active.
# [END OF COMMAND SCRIPT]
# ==============================================================================
