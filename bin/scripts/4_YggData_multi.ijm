//YggData is the ThompsonLab ImageJ macro for single cell analysis of IFA images

runPeri=false;
enlargeFactor="3";
nucleusSize="30-200";
dnaMinThreshold=48;

runWC=true;
wcTarget1="dna";
wcTarget1Threshold=dnaMinThreshold;
wcTarget1Size=nucleusSize;

wcTarget2="ns3";
wcTarget2Threshold=15;
wcTarget2Size="1-Infinty";

wcTarget3="NA";
wcTarget3Threshold=22;
wcTarget3Size="1-Infinty";

wcTarget4="ccne";
wcTarget4Threshold=25;
wcTarget4Size="1-Infinty";

wcTarget5="tag";
wcTarget5Threshold=15;
wcTarget5Size="1-Infinty";

wcTargetDefaultSize="1-Infinty";

//	This macro first asks you to pick a nucleus picture to generate ROIs from
//	It then uses these ROIs to determine the raw NUCLEAR data from each image within the directory, and stores these CSVs in a newly created 'Nuclear' folder
//	Ygg will then back out and generate ROIs for each image independent of nuclear localization, and store these CSVs in a newly created 'WholeCell' folder
//	Following this macro, the imaGen() script to combine all the data into a new dataset in which each row is a single cell

// First, lets make sure the measurements are set for appropriate analyses. If changes are made here, they will not be included in the default imaGen() compilation

run("Set Measurements...", "area mean standard modal min centroid center perimeter feret's integrated median area_fraction display redirect=None decimal=9");

roi_image = File.openDialog("Choose a File");
input=getDirectory("current");
parent=File.getParent(input);
input=File.getParent(parent);
input=File.getParent(input);

pList=getFileList(input);
for (i=0; i < pList.length; i++){
	if(!endsWith(pList[i], ".ijm")){
		//print(pList[i]);
		dList=getFileList(input+"/"+pList[i]);
		for (j=0; j < dList.length; j++){
			pathway = input+"/"+pList[i]+"/"+dList[j]+"/PNGS";
			iList = getFileList(pathway);
			roi_image = pathway+"/"+"dna.png";
			open(roi_image);
			run("8-bit");
			setAutoThreshold("Default dark");
			//run("Threshold...");
			setThreshold(dnaMinThreshold, 255);
			setOption("BlackBackground", false);
			run("Convert to Mask");
//The ROIs are rounded and split
// If you are running a high magnification (>10x) DNA image, it is recommended that you comment this out to avoid nuclear image fragementation
			run("Watershed");
//The ROIs are generated
			run("Analyze Particles...", "size="+nucleusSize+" add include exclude");
//The image is closed
			close();			
			
// Then you generate a list of the images in that directory:
			if(!File.isDirectory(pathway+"/Nuclear")){
				File.makeDirectory(pathway+"/Nuclear");
			};
			inputn = pathway+"/Nuclear/";
			for (k=0; k < iList.length; k++){
				if (endsWith(iList[k], ".png") & !startsWith(iList[k], "overlay")){
					open(pathway+"/"+iList[k]);
					roiManager("measure");
					saveAs("Results", inputn+replace(iList[k], ".png", ".csv"));
					run("Clear Results");
					selectWindow("Results");
					run("Close");
					close();
				};
			};
			selectWindow("ROI Manager");
			run("Close");
			if(runPeri){
// Now the ROIs re-drawn and are increased by an enlargment factor
				if(!File.isDirectory(pathway+"/Perinuclear")){
					File.makeDirectory(pathway+"/Perinuclear");
				};
				inputp = pathway+"/Perinuclear/";
				open(roi_image);
				run("8-bit");
				setAutoThreshold("Default dark");
				setThreshold(dnaMinThreshold, 255);
				setOption("BlackBackground", false);
				run("Convert to Mask");
//The ROIs are rounded and split
				run("Watershed");
				counts=roiManager("count");
	
				for(l=0; l<counts; l++) {
    				roiManager("Select", l);
   				 	run("Enlarge...", "enlarge="+enlargeFactor);
    				roiManager("Update");
				};
				roiManager("deselect");
				close();
			// And the images are re-analyzed with slightly larger ROIs
				for (k=0; k < iList.length; k++){
					if (endsWith(iList[k], ".png") & !startsWith(iList[k], "overlay")){
						open(pathway+"/"+iList[k]);
						roiManager("measure");
						saveAs("Results", inputp+replace(iList[k], ".png", ".csv"));
						run("Clear Results");
						selectWindow("Results");
						run("Close");
						close();
					};
				};
			//Then everything is closed
				selectWindow("ROI Manager");
				run("Close");
			};
//Now that the nuclear data is collected, Ygg will collected the nuclear indepedent data
//Since the list of images will be the same, it simply iterates through each image and collects the total ROI pixel data
			if(runWC){
				if(!File.isDirectory(pathway+"/WholeCell")){
					File.makeDirectory(pathway+"/WholeCell");
				};
				inputc = pathway+"/WholeCell/";
				for (k=0; k < iList.length; k++){
					if (endsWith(iList[k], ".png") & !startsWith(iList[k], "overlay")){
						open(pathway+"/"+iList[k]);
						run("8-bit");
// Change this for deviations from default
						setAutoThreshold("Default dark");
//run("Threshold...");
						if (startsWith(iList[k], wcTarget1)){
							setThreshold(dnaMinThreshold, 255);
							setOption("BlackBackground", false);
							run("Convert to Mask");
							run("Watershed");
							run("Analyze Particles...", "size="+nucleusSize+" include add exclude");
						} else if (startsWith(iList[k], wcTarget2)){
							setThreshold(wcTarget2Threshold, 255);
							setOption("BlackBackground", false);
							run("Convert to Mask");
							run("Watershed");
							run("Analyze Particles...", "size="+wcTarget2Size+" add");
						} else if (startsWith(iList[k], wcTarget3)){
							setThreshold(wcTarget3Threshold, 255);
							setOption("BlackBackground", false);
							run("Convert to Mask");
							run("Watershed");
							run("Analyze Particles...", "size="+wcTarget3Size+" add");
						} else if (startsWith(iList[k], wcTarget4)){
							setThreshold(25, 255);
							setOption("BlackBackground", false);
							run("Convert to Mask");
							run("Watershed");
							run("Analyze Particles...", "size="+wcTarget4Size+" add");
						} else if (startsWith(iList[k], wcTarget5)){
							setThreshold(wcTarget5Threshold, 255);
							setOption("BlackBackground", false);
							run("Convert to Mask");
							run("Watershed");
							run("Analyze Particles...", "size="+wcTarget5Size+" add");
						}else {
							setAutoThreshold("Default dark");
							setOption("BlackBackground", false);
							run("Convert to Mask");
							run("Watershed");
							run("Analyze Particles...", "size="+wcTargetDefaultSize+" add");
						};
						close();
						open(pathway+"/"+iList[k]);
						roiManager("measure");
						saveAs("Results", inputc+replace(iList[k], ".png", ".csv"));
						run("Clear Results");
						run("Close");
						roiManager("reset")
						if (nImages>0) {
							close();
						};
					};
				};
//Everything is closed
			selectWindow("ROI Manager");
			run("Close");
			};
		};
	};
}
