# Define the shell
SHELL := /bin/bash
# R script command
R_SCRIPT := Rscript
# Output directories
OUTPUT_DIR := ./output/Braunschweig/
RESULTS_DIR := $(OUTPUT_DIR)/results
FIG_OUTPUT_DIR := $(OUTPUT_DIR)/figures
# Script locations
DOWNLOAD_DATA := src/simulation/download-input-data.R
PERF_SIM_SCRIPT := src/simulation/run-performance-evaluation-simulation.R
PARAM_SIM_SCRIPT := src/simulation/run-param-space-simulation.R
SUMMARIZE_PARAM_SCRIPT := src/simulation/summarize-param-space-simulation.R
SUMMARIZE_PERF_SCRIPT := src/simulation/summarize-perf-evaluation.R
# Input and simulation results data
INPUT_DIR := input/
PARAM_SIM_OUTPUT := $(RESULTS_DIR)/param_space_sim.rds
PERF_SIM_OUTPUT := $(RESULTS_DIR)/perf_eval_sim.rds
# Figure files and their corresponding scripts
FIG1_FILE := $(FIG_OUTPUT_DIR)/spectra-and-noise-tf.pdf
FIG2_FILE := $(FIG_OUTPUT_DIR)/comparison-against-REA-co2-and-diurnal.pdf
FIG3_FILE := $(FIG_OUTPUT_DIR)/nondim-uncertainty.pdf
FIG4_FILE := $(FIG_OUTPUT_DIR)/parameter-heatmap.pdf
FIG5_FILE := $(FIG_OUTPUT_DIR)/wind-C-reduction-maximization.png
FIG6_FILE := $(FIG_OUTPUT_DIR)/valve-opentimes-distribution.pdf
FIG1_SCRIPT := src/plots/1-noise-shaping.R
FIG2_SCRIPT := src/plots/2-performance-evaluation-QEA.R
FIG3_SCRIPT := src/plots/3-nondimensional-uncertainty.R
FIG4_SCRIPT := src/plots/4-parameter-space.R
FIG5_SCRIPT := src/plots/5-nonzero-w-and-deltaC.R
FIG6_SCRIPT := src/plots/6-valve-switching-dynamics.R

# Default target
all: simulations figures 

# Create all required directories if they don't exist
.PHONY: directories
directories:
	mkdir -p $(OUTPUT_DIR)
	mkdir -p $(RESULTS_DIR)
	mkdir -p $(FIG_OUTPUT_DIR)

# Download input data
$(INPUT_DIR): directories $(DOWNLOAD_DATA)
	@echo "Downloading data..."
	$(R_SCRIPT) $(DOWNLOAD_DATA)
	@touch $(INPUT_DIR)  # Update timestamp of the directory

# Simulation and summarization for performance evaluation
$(PERF_SIM_OUTPUT): $(PERF_SIM_SCRIPT) $(INPUT_DIR)
	$(R_SCRIPT) $(PERF_SIM_SCRIPT) && $(R_SCRIPT) $(SUMMARIZE_PERF_SCRIPT)

# Simulation and summarization for parameter space analysis
$(PARAM_SIM_OUTPUT): $(PARAM_SIM_SCRIPT) $(INPUT_DIR)
	$(R_SCRIPT) $(PARAM_SIM_SCRIPT) && $(R_SCRIPT) $(SUMMARIZE_PARAM_SCRIPT)

# Simulation target now explicitly depends on the input data being downloaded
simulations: $(PERF_SIM_OUTPUT) $(PARAM_SIM_OUTPUT)

# Figure creation targets
$(FIG1_FILE): $(FIG1_SCRIPT)
	$(R_SCRIPT) $(FIG1_SCRIPT)

$(FIG2_FILE): $(FIG2_SCRIPT)
	$(R_SCRIPT) $(FIG2_SCRIPT)

$(FIG3_FILE): $(FIG3_SCRIPT)
	$(R_SCRIPT) $(FIG3_SCRIPT)

$(FIG4_FILE): $(FIG4_SCRIPT)
	$(R_SCRIPT) $(FIG4_SCRIPT)

$(FIG5_FILE): $(FIG5_SCRIPT)
	$(R_SCRIPT) $(FIG5_SCRIPT)

$(FIG6_FILE): $(FIG6_SCRIPT) 
	$(R_SCRIPT) $(FIG6_SCRIPT)

# Custom target to build all figures
figures: $(FIG1_FILE) $(FIG2_FILE) $(FIG3_FILE) $(FIG4_FILE) $(FIG5_FILE) $(FIG6_FILE)

.PHONY: summarize
summarize: 
	$(R_SCRIPT) $(SUMMARIZE_PERF_SCRIPT) && $(R_SCRIPT) $(SUMMARIZE_PARAM_SCRIPT)

# Clean up
.PHONY: clean
clean:
	rm -rf $(OUTPUT_DIR)*
	rm -rf $(FIG_OUTPUT_DIR)*

.PHONY: all figures simulations clean directories
