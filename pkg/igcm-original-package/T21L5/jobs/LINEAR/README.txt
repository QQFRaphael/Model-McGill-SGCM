

Linear Model

1) run FORCE+edd0.job first
Analyze the output from /zemo2/jiaxj/igcm/T21L5/EXPERIMENTS/LINEAR-Aanlysis use sh 1getforceLinear.sh
to get the forcing used for the linearized SGCM, the input initial condition is 51 years averaged DJF


2)run PERT+E1LinearNAO.job  --  linearized model
Before run this job, we need to construct the forcing anomaly first. this anomaly is input to the model
through fort.15.
the file has the spectra format.



###################

