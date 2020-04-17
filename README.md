# Data analysis

These files give the analysis done for the final paper of the course Experimentation in Psychology and Linguistics in 2020.

## Files

The files [results_fr.csv](results_fr.csv) and [results_nl.csv](results_nl.csv) give the raw output from Ibex Farm, which was used to run the experiment. [preprocessing.R](preprocessing.R) contains the code to convert the results into a more readable table, which is exported to [exp.data.Rdata](exp.data.Rdata).

To compare responses to the original slogan that the participant saw, the file [slogans.csv](slogans.csv) contains the original slogans. [check_recall_correctness.R](check_recall_correctness.R) adds data on correctness, and exports it to [exp_data_with_correctness.Rdata](exp_data_with_correctness.Rdata).

The [analysis.R](analysis.R) file contains the analysis of French data, while [analysis_dutch.R](analysis_dutch.R) contains the analysis of Dutch data.

Assignment made by:
Luka van der Plas
Valentin Richard
