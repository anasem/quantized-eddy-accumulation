SHELL := /bin/bash
R_SCRIPT = Rscript
OUTPUT_DIR = ./output/results/
FIG_OUTPUT_DIR = ./output/figures
DOWNLOAD_DATA = src/simulation/download-input-data.R
SIM_SCRIPT =  src/simulation/run-best-case-senario.R
PARAM_SIM_SCRIPT = src/simulation/run-param-space-simulation-mc.R
SUMMARY_SCRIPT = src/simulation/summarize-sim-results.R
INPUT_DATA = input/raw_data_input.rds
SIM_OUTPUT = $(OUTPUT_DIR)/sim_perf_eval.rds
PARAM_SIM_OUTPUT = $(OUTPUT_DIR)/sim_param_spc.rds
SUMMARY_OUTPUT = $(OUTPUT_DIR)/results_tbl.rds
FIG2_PERF_EVAL = $(FIG_OUTPUT_DIR)/comparison-against-REA-co2-and-diurnal.pdf
FIG2_SCRIPT=src/plots/2-performance-evaluation-QEA.R
FIG1_TF = $(FIG_OUTPUT_DIR)/spectra-and-noise-tf.pdf
FIG1_SCRIPT=src/plots/1-noise-shaping.R
FIG3_PARAM_SPACE = $(FIG_OUTPUT_DIR)/parameter-heatmap.png
FIG3_SCRIPT=src/plots/3-parameter-space.R
FIG4_WC = $(FIG_OUTPUT_DIR)/wind-C-reduction-maximization.png
FIG4_SCRIPT=src/plots/4-nonzero-w-and-deltaC.R
FIG5_VALVE=$(FIG_OUTPUT_DIR)/valve-opentimes-distribution.pdf
FIG5_SCRIPT=src/plots/5-valve-switching-dynamics.R

# Default target
all: $(FIG1_TF) $(FIG2_PERF_EVAL) $(FIG3_PARAM_SPACE) $(FIG4_WC) $(FIG5_VALVE)

.PHONY: directories
directories:
	mkdir -p $(OUTPUT_DIR)
	mkdir -p $(FIG_OUTPUT_DIR)

# Download input data - File target
$(INPUT_DATA): directories $(DOWNLOAD_DATA)
	@echo "Downloading data..."
	$(R_SCRIPT) $(DOWNLOAD_DATA)

# Run simulation - File target
$(SIM_OUTPUT): directories $(SIM_SCRIPT) $(INPUT_DATA)
	$(R_SCRIPT) $(SIM_SCRIPT)

# Run parameter space simulation - File target
$(PARAM_SIM_OUTPUT): directories $(PARAM_SIM_SCRIPT) $(INPUT_DATA)
	$(R_SCRIPT) $(PARAM_SIM_SCRIPT)

# Summarize simulation results - File target
$(SUMMARY_OUTPUT): directories $(SUMMARY_SCRIPT) $(SIM_OUTPUT) $(PARAM_SIM_OUTPUT)
	$(R_SCRIPT) $(SUMMARY_SCRIPT)

# Create figure 1 - File target
$(FIG1_TF): directories $(FIG1_SCRIPT) $(INPUT_DATA)
	$(R_SCRIPT) $(FIG1_SCRIPT)

# Create figure 2 - File target
$(FIG2_PERF_EVAL): directories $(FIG2_SCRIPT) $(SUMMARY_OUTPUT)
	$(R_SCRIPT) $(FIG2_SCRIPT)


# Create figure 3 - File target
$(FIG3_PARAM_SPACE): directories $(FIG3_SCRIPT) $(INPUT_DATA)
	$(R_SCRIPT) $(FIG3_SCRIPT)

# Create figure 4 - File target
$(FIG4_WC): directories $(FIG4_SCRIPT) $(INPUT_DATA)
	$(R_SCRIPT) $(FIG4_SCRIPT)

# Create figure 5 - File target
$(FIG5_VALVE): directories $(FIG5_SCRIPT) $(INPUT_DATA)
	$(R_SCRIPT) $(FIG5_SCRIPT)

# Custom target for creating figure 2
fig2: $(FIG2_PERF_EVAL)

# Custom target, create all figures
figures: $(FIG1_TF) $(FIG2_PERF_EVAL) $(FIG3_PARAM_SPACE) $(FIG4_WC) $(FIG5_VALVE)

simulations: $(SIM_OUTPUT) $(PARAM_SIM_OUTPUT) $(SUMMARY_OUTPUT)
perfsim: $(SIM_OUTPUT) $(SUMMARY_OUTPUT)

.PHONY: all clean
clean:
	rm -vrf $(OUTPUT_DIR)/*
	rm -vrf $(FIG_OUTPUT_DIR)/*

.PHONY: all 
