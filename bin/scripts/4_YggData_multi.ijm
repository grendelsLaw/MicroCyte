//YggData is the ThompsonLab ImageJ macro for single cell analysis of IFA images

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
//print(input);
//x=split(input, "/");
//y=""
//for(h=0; h<x.length-3; h++){
//	y=y+"/"+x[h];
//};
//print(y);
//input=y;
pList=getFileList(input);
for (i=0; i < pList.length; i++){
	//print(pList[i]);
	if(!endsWith(pList[i], ".ijm")){
		dList=getFileList(input+"/"+pList[i]);
		for (j=0; j < dList.length; j++){
			pathway = input+"/"+pList[i]+"/"+dList[j]+"/PNGS";
			iList = getFileList(pathway);
			roi_image = pathway+"/"+"dna.png";
			open(roi_image);
			run("8-bit");
			setAutoThreshold("Default dark");
			//run("Threshold...");
			setThreshold(30, 255);
			setOption("BlackBackground", false);
			run("Convert to Mask");
//The ROIs are rounded and split
// If you are running a high magnification (>10x) DNA image, it is recommended that you comment this out to avoid nuclear image fragementation
			run("Watershed");
//The ROIs are generated
			run("Analyze Particles...", "size=30-333 add include exclude");
//The image is closed
			close();
// Then a Nuclear and WholeCell directory is made, if not already present
			if(!File.isDirectory(pathway+"/Nuclear")){
				File.makeDirectory(pathway+"/Nuclear");
			};
			if(!File.isDirectory(pathway+"/WholeCell")){
				File.makeDirectory(pathway+"/WholeCell");
			};
			inputn = pathway+"/Nuclear/";
			inputc = pathway+"/WholeCell/";
// Then you generate a list of the images in that directory:
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
//Then everything is closed
			selectWindow("ROI Manager");
			run("Close");
//Now that the nuclear data is collected, Ygg will collected the nuclear indepedent data
//Since the list of images will be the same, it simply iterates through each image and collects the total ROI pixel data
			for (k=0; k < iList.length; k++){
				if (endsWith(iList[k], ".png") & !startsWith(iList[k], "overlay")){
					open(pathway+"/"+iList[k]);
					run("8-bit");
// Change this for deviations from default
					setAutoThreshold("Default dark");
//run("Threshold...");
					if (startsWith(iList[k], "dna")){
						setThreshold(30, 255);
						setOption("BlackBackground", false);
						run("Convert to Mask");
						run("Watershed");
						run("Analyze Particles...", "size=30-333 include add exclude");
					} else if (startsWith(iList[k], "mcm4")){
						setThreshold(16, 255);
						setOption("BlackBackground", false);
						run("Convert to Mask");
						run("Watershed");
						run("Analyze Particles...", "size=1-Infinity include add");
					} else if (startsWith(iList[k], "edu")){
						setThreshold(15, 255);
						setOption("BlackBackground", false);
						run("Convert to Mask");
						run("Watershed");
						run("Analyze Particles...", "size=20-333 include add");
					} else if (startsWith(iList[k], "cyclinD")){
						setThreshold(20, 255);
						setOption("BlackBackground", false);
						run("Convert to Mask");
						run("Watershed");
						run("Analyze Particles...", "size=1-Infinity include add");
					}else {
						setAutoThreshold("Default dark");
						setOption("BlackBackground", false);
						run("Convert to Mask");
						run("Watershed");
						run("Analyze Particles...", "size=1-Infinity include add");
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
}
