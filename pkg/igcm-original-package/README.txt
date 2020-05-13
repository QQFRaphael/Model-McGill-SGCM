
RUN jobs from zemo


MODEL.job --- to get FORCE fields

CONTROL.job ---control run using 51 DJF averaged forcing.


getDH.job -- to get the total diabatic forcing  (FORCE+DAMPING)
		RESTIM=0. is set.


BUDGET.job -- to get the total diabatic forcing (MODEL FORCE+DAMPING) 
		(use the temperature field:T+250)
		(/D1/hlin/igcm/data/UVTP4849after.dat.spec_corrected+250)

Linear Model
FORCE+edd0.job -- to get the FORCE+eddy,input is 51 years averaged DJF
PERT+E1LinearNAO.job  --  linearized model

###################
Note: There is a maximum integration steps set in the job, if you want
	to integrate a longer time, you must reset the value:
	KTRAIN.LT.90*51


#################

Dir: ./EXPE  is used to do integration using 51 different forcings

Dir ./ExpIdeal  is used to do integration using one forcing 

Dir ./Forecast  is used to do forecast for the setup process of the AO/NAO

Dir ./Prediction  is used to get the damp.exe, which is modified for above forecast experiment.



