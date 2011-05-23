StatET for R Plugin: http://www.walware.de/goto/statet

Installation:
1. Install plugin from one of the repositories:
	For Eclipse 3.7:
		http://download.walware.de/eclipse-3.7
	For Eclipse 3.6:
		http://download.walware.de/eclipse-3.6
	For Eclipse 3.5:
		http://download.walware.de/eclipse-3.5
	For Eclipse 3.4:
		http://download.walware.de/eclipse-3.4

2. Install rJava and rj packages for R
	In R run command:
		install.packages('rJava',,'http://www.rforge.net/')
		install.packages('rj', repos="http://download.walware.de/rj-0.5")

3. Install libraries used in the project
	In R run command:
		install.packages(c('lsa','klaR'))

4. In Eclipse set up the R Environment:
	4.1. Go to Window->Preferences->StatET->Run/Debug->R Environments
	4.2. Click Add->local
	4.3. Fill in the "Location (R_HOME)", you can also click on the "+" sign on the right and make eclipse try to find it automatically
	4.4. Click "Detect Default Properties/Settings"
	4.5. Click OK

5. Create an Eclipse Run Configuration to launch R Console
	5.1. Go to Run->Run Configurations...
	5.2. Create a new configuration for the R Console
	5.3. Properties to set:
			Main tab->Launch Type : RJ
			R Console tab->R Snippet Run on Startup: (load libraries needed by the project)
				library(lsa)
				library(klaR)
	5.4. Apply and Run the configuration.
	5.5. (R should start indexing the environment for future code completion, wchich can take a couple minutes)
	
6. Run a script in the R Console
	6.1. Open an R script in Eclipse
	6.2. Press Ctrl+R for a StatET actions menu
	6.3. Double click "Run File in R via Command and Go to Console" (or press S)

7. Enjoy :)