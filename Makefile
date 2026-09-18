# Makefile — exchange rate response to news
# Run `make` to build all outputs; `make paper` to also render the manuscript.

# ── Shared dependencies ───────────────────────────────────────────────────────
PACKAGES      = packages.R
FX_PLOT       = Functions/fx_plot.R
MODEL_FNS     = Functions/model_functions.R
SHARED        = $(PACKAGES) $(FX_PLOT)

# ── Output paths ──────────────────────────────────────────────────────────────
OUT           = Outputs

# ── Top-level targets ─────────────────────────────────────────────────────────
.PHONY: all paper clean

all: \
	$(OUT)/artifacts_sa_news_models.rds \
	$(OUT)/artifacts_sa_residual_models.rds \
	$(OUT)/artifacts_us_news_models.rds \
	$(OUT)/artifacts_us_residual_models.rds \
	$(OUT)/artifacts_differntial_models.rds

paper: all
	quarto render exchange_rate_response_to_news.qmd

# ── Stage 1: Independent data inputs ─────────────────────────────────────────
$(OUT)/artifacts_market_based_surprises.rds: Scripts/01_sa_surprises.R $(SHARED)
	Rscript $<

$(OUT)/artifacts_eme_surprises.rds: Scripts/02_eme_surprises.R $(SHARED)
	Rscript $<

$(OUT)/artifacts_us_surprises.rds: Scripts/03_us_surprises.R $(SHARED)
	Rscript $<

$(OUT)/artifacts_spot_and_spot_forwards_daily.rds: Scripts/04_daily_spot_forward.R $(SHARED)
	Rscript $<

$(OUT)/artifacts_interest_rate.rds: Scripts/05_interest_rate.R $(SHARED)
	Rscript $<

$(OUT)/artifacts_spot_spot_forwards_hourly.rds: Scripts/spot_and_spot_forward_hourly.R $(SHARED)
	Rscript $<

# ── Stage 2: Joint response data ──────────────────────────────────────────────
$(OUT)/artifacts_joint_response_data.rds: \
	Scripts/06_joint_response_calc.R \
	$(OUT)/artifacts_interest_rate.rds \
	$(OUT)/artifacts_spot_and_spot_forwards_daily.rds \
	$(SHARED)
	Rscript $<

# ── Stage 3: Model data ───────────────────────────────────────────────────────
$(OUT)/artifacts_model_data.rds: \
	Scripts/07_model_data.R \
	$(OUT)/artifacts_joint_response_data.rds \
	$(OUT)/artifacts_market_based_surprises.rds \
	$(OUT)/artifacts_eme_surprises.rds \
	$(OUT)/artifacts_us_surprises.rds \
	$(SHARED)
	Rscript $<

# ── Stage 4: Models ───────────────────────────────────────────────────────────
$(OUT)/artifacts_sa_news_models.rds: \
	Scripts/08_sa_news_models.R \
	$(OUT)/artifacts_model_data.rds \
	$(MODEL_FNS) $(SHARED)
	Rscript $<

$(OUT)/artifacts_sa_residual_models.rds: \
	Scripts/09_sa_residual_models.R \
	$(OUT)/artifacts_model_data.rds \
	$(MODEL_FNS) $(SHARED)
	Rscript $<

$(OUT)/artifacts_us_news_models.rds: \
	Scripts/10_us_news_models.R \
	$(OUT)/artifacts_model_data.rds \
	$(MODEL_FNS) $(SHARED)
	Rscript $<

$(OUT)/artifacts_us_residual_models.rds: \
	Scripts/11_us_residual_models.R \
	$(OUT)/artifacts_model_data.rds \
	$(MODEL_FNS) $(SHARED)
	Rscript $<

$(OUT)/artifacts_differntial_models.rds: \
	Scripts/12_interest_rate_models.R \
	$(OUT)/artifacts_model_data.rds \
	$(MODEL_FNS) $(SHARED)
	Rscript $<

# ── Housekeeping ──────────────────────────────────────────────────────────────
clean:
	rm -f $(OUT)/artifacts_*.rds
