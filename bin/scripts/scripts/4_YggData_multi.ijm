//YggData is the ThompsonLab ImageJ macro for single cell analysis of IFA images
anchorName="dna";
anchorSize="30-400";
anchorMinThreshold=45;
circularity=0.75

runPeri=false;
enlargeFactor="3";

runWC=false;
wcTarget1="dna";
wcTarget1Threshold=anchorMinThreshold;
wcTarget1Size=anchorSize;

wcTarget2="n";
wcTarget2Threshold=19;
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
//	It then uses these ROIs to determine the raw Anchor_extraction data from each image within the directory, and stores these CSVs in a newly created 'Anchor_extraction' folder
//	Ygg will then back out and generate ROIs for each image independent of Anchor_extraction localization, and store these CSVs in a newly created 'NonAnchor_extraction' folder
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
			roi_image = pathway+"/"+anchorName+".png";
			//print(roi_image);
			if(File.exists(roi_image)){
					open(roi_image);
				run("8-bit");
				setAutoThreshold("Default dark");
//run("Threshold...");
				setThreshold(anchorMinThreshold, 255);
				setOption("BlackBackground", false);
				run("Convert to Mask");
//The ROIs are rounded and split
// If you are running a high magnification (>10x) DNA image, it is recommended that you comment this out to avoid Anchor_extraction image fragementation
				run("Watershed");
//The ROIs are generated
				run("Analyze Particles...", "size="+anchorSize+" add include exclude circularity="+circularity+"-1.00");
//The image is closed
				close();			
			
// Then you generate a list of the images in that directory:
				if(!File.isDirectory(pathway+"/Anchor_extraction")){
					File.makeDirectory(pathway+"/Anchor_extraction");
				};
				inputn = pathway+"/Anchor_extraction/";
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
			
				if(runPeri){
// Now the ROIs re-drawn and are increased by an enlargment factor
					if(!File.isDirectory(pathway+"/PeriAnchor_extraction")){
						File.makeDirectory(pathway+"/PeriAnchor_extraction");
					};
					inputp = pathway+"/PeriAnchor_extraction/";
					open(roi_image);
					run("8-bit");
					setAutoThreshold("Default dark");
					setThreshold(anchorMinThreshold, 255);
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
				} else {
					selectWindow("ROI Manager");
					run("Close");
				};
			
//Now that the Anchor_extraction data is collected, Ygg will collected the Anchor_extraction indepedent data
//Since the list of images will be the same, it simply iterates through each image and collects the total ROI pixel data
				if(runWC){
					if(!File.isDirectory(pathway+"/NonAnchor_extraction")){
						File.makeDirectory(pathway+"/NonAnchor_extraction");
					};
					inputc = pathway+"/NonAnchor_extraction/";
					for (k=0; k < iList.length; k++){
						if (endsWith(iList[k], ".png") & !startsWith(iList[k], "overlay")){
							open(pathway+"/"+iList[k]);
							run("8-bit");
// Change this for deviations from default
							setAutoThreshold("Default dark");
//run("Threshold...");
							if (startsWith(iList[k], wcTarget1)){
								setThreshold(anchorMinThreshold, 255);
								setOption("BlackBackground", false);
								run("Convert to Mask");
								run("Watershed");
								run("Analyze Particles...", "size="+anchorSize+" include add exclude");
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
			}else{
				print("Anchor image not found in "+pList[i]+" - "+dList[j]);
			};
		};
	};
}
