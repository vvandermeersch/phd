package capsis.lib.castanea.phenologyrunner;

import java.io.Serializable;

import capsis.lib.castanea.FmCanopy;
import capsis.lib.castanea.FmCell;
import capsis.lib.castanea.FmClimateDay;
import capsis.lib.castanea.FmPhenology;
import capsis.lib.castanea.FmSettings;
import capsis.lib.castanea.FmSpecies;
import capsis.lib.castanea.FmFluxesResults;
import capsis.lib.castanea.phenologyrunner.fitlibfunction.FmFitlibPhenoFunction;
import capsis.lib.phenofit.FitlibConstants;
import capsis.lib.phenofit.FitlibDailyMemory;
import capsis.lib.phenofit.FitlibPhenology;
import capsis.lib.phenofit.climateloader.FitlibClimate;
import capsis.lib.phenofit.fit2018.Fit2018Pheno;
import capsis.lib.phenofit.fit2018.Fit2018PhenoManager;
import capsis.util.Fit2018Date;
import jeeb.lib.util.Log;

/**
 * FmPhenologyRunner: phenology of leaves and flowers.
 *
 * @author Hendrik Davi - october 2009
 */
public abstract class FmPhenologyRunner implements Serializable, Cloneable {

	private static boolean leafModelHeaderWritten;
	private static boolean flowerModelHeaderWritten;
	private static boolean fruitModelHeaderWritten;
	private static boolean pollenModelHeaderWritten;

	private static boolean trace_leafSenescenceEffectHeaderWritten;

	// fc+hd-21.6.2018 reviewed
	protected FmCell cell;
	protected FmSettings settings;
	protected FmSpecies species;

	// Common phenological variables

	protected int BBDAY; // budburst date
	protected int BBDAY2; // budburst date of second flush

	protected int LMAXN = 366; // date when LAI is maximum
	protected int LMAXN2 = 366;
	protected int LFALL = 366;
	protected double LFALLH;
	protected int LEND = 366;
	protected int DAYLMA;
	protected double TSUM;
	protected double TSUM2;
	protected double TLMA;
	protected double TLMA2;
	protected double HSUMLA;
	protected double HSUMLA2;
	protected double HSUMLFAL;
	protected double TSUMFALD;
	protected double dayOfWoodStop = 366;
	protected double dayOfEndFall;
	protected double dayOfBeginFall;
	protected double sumLstress;
	protected double dayOfLateFrost;
	protected double LMAXsecondFlush;
	protected double Rch;
	protected double Sch;
	protected double Rfr;
	protected double Sfr;
	protected double diffLAIFrost;
	protected double LwithoutFrost; // fc-hd-21.6.2018


	// Parameters from phenofit model

	protected int leafDormancyBreakDate;
	protected int leafUnfoldingDate;
	protected int leafSenescenceDate;
	protected int flowerDormancyBreakDate;
	protected int floweringDate;
	protected int fruitGrowthInitDate;
	protected int fruitMaturationDate;
	protected int pollinationDate;

	protected double leafFrostDamage;
	protected double memoryLeafFrostDamage;
	protected double leafFrostHardiness;

	// fc+vj-4.9.2018 replaced FmFitlib methods by objects
	protected FmFitlibPhenoFunction leafModel;
	protected FmFitlibPhenoFunction flowerModel;
	protected FmFitlibPhenoFunction fruitModel;
	protected FmFitlibPhenoFunction pollenModel;

	// senesence model delpierre FitlibSenDelpierre
// 
	protected double senescence_Pb; //DStart for Delpierre
	protected double senescence_Tb; // TBASED for delpierre
	protected double senescence_alpha; // Xt for delpierre
	protected double senescence_beta; // xH for delpierre
	protected double senescence_Scrit; //TSUMYELO
	protected double senescence_sigmaSen;

	// fc-10.2.2020 This boolean is managed partly here and partly in
	// subclasses. It is supposed to be false between 1 January and budburst,
	// true otherwise
	protected boolean budburstAchieved;

	/**
	 * Constructor.
	 */
	public FmPhenologyRunner(FmCell cell, FmSettings settings) throws Exception {

		this.cell = cell;
		this.settings = settings;
		this.species = cell.getSpecies();

	}

	/**
	 * Runs the phenology on the given cell with the given climate for day j,
	 * writes results in yearlyResults.
	 */
	public abstract void execute(FmFluxesResults yearlyResults, FmClimateDay climateDay, int j) throws Exception;

	/**
	 * Phenology, Fitlib method
	 */
	protected FmPhenology phenoFitlib(int day, FmClimateDay climateDay, boolean firstYearOfSimulation)
			throws Exception {

		// fc+vj-4.9.2018 renamed method, was phenoFitlib, clearly deals with
		// leaves

		// Create a new pheno if 1 sept // fc+hd+nb-28.11.2016
		int sept1 = FitlibClimate.get1September(cell.currentYear);

		if (day == sept1) {
			boolean deciduous = true;
			FmPhenology pheno = new FmPhenology(cell.currentYear, deciduous);
			cell.addPhelibPheno(pheno);

		}
		
		// fc-10.2.2020 Reset budburstAchieved on 1 January
		if (day == 1)
			budburstAchieved = false;

		// Process current day for all phenos, search pheno carrying the
		// unfolding date // fc+hd+nb-28.11.2016
		FmPhenology phenoOfInterest = null;

		for (FmPhenology pheno : cell.getPhelibPhenoList()) {
			if (pheno.getBirthYear() == cell.currentYear - 1)
				phenoOfInterest = pheno; // will carry unfolding date

			// fc+vj-4.9.2018 Fitlib pheno functions are no objects
			leafModel.run(cell, climateDay, cell.currentYear, day, pheno);

			if (settings.output > 1) {
				Log.println("leafModel",
						leafModel.getName() + ";" + cell.currentYear + ";" + cell.getId() + ";" + day + ";"
								+ pheno.getBirthYear() + ";" + pheno.getLeafDormancyBreakDate() + ";" + BBDAY + ";"
								+ sept1);
			}

			calculateLeafSenescence(cell, pheno, climateDay, day);

			calculateLeafSenescenceEffect(cell, pheno, day);

			cell.leafFrost.execute(cell, settings, climateDay, day, pheno);

			calculateLeafIndex(pheno, cell.currentYear, day);

			// fc+vj-5.9.2018 trace pb fructification date2
			// if (!trace_leafSenescenceEffectHeaderWritten) {
			// Log.println("trace_leafSenescenceEffect",
			// "cellId;year;day;frostDammage;senescenEffect;leafIndex");
			// trace_leafSenescenceEffectHeaderWritten = true;
			// }
			Log.println("trace_leafSenescenceEffect",
					"" + cell.getId() + ";" + cell.currentYear + ";" + day + ";" + pheno.leafMemory.frostDamage + ";"
							+ pheno.leafMemory.senescenceEffect + ";" + pheno.getLeafIndex());

			// FitlibMemory leaMemory = pheno.leafMemory; // fc-5.9.2018 to ease
			// debugging

			// fc+vj-4.9.2018 flowerModel is optional (see initPhenoFitlib ())
			if (flowerModel != null) {
				flowerModel.run(cell, climateDay, cell.currentYear, day, pheno);

				if (settings.output > 1) {
					Log.println("flowerModel",
							flowerModel.getName() + ";" + cell.currentYear + ";" + cell.getId() + ";" + day + ";"
									+ pheno.getBirthYear() + ";" + pheno.getFlowerDormancyBreakDate() + ";"
									+ pheno.getFloweringDate() + ";" + sept1);
				}

			}

			// The leafIndex must be calculated before fruitMatureation, see
			// Fit5Model
			calculateLeafIndex(pheno);

			// fc+vj-4.9.2018 fruitModel is optional (see initPhenoFitlib ())
			if (fruitModel != null) {
				fruitModel.run(cell, climateDay, cell.currentYear, day, pheno);

				if (settings.output > 1) {
					Log.println("fruitModel",
							fruitModel.getName() + ";" + cell.currentYear + ";" + cell.getId() + ";" + day + ";"
									+ pheno.getBirthYear() + ";" + pheno.getFruitGrowthInitDate() + ";"
									+ pheno.getFruitMaturationDate() + ";" + sept1);
				}

			}

			// fc+vj-5.9.2018 pollenModel is optional (see initPhenoFitlib ())
			if (pollenModel != null) {
				pollenModel.run(cell, climateDay, cell.currentYear, day, pheno);

				if (settings.output > 1) {
					Log.println(settings.logPrefix + "pollenModel",
							pollenModel.getName() + ";" + cell.currentYear + ";" + cell.getId() + ";" + day + ";"
									+ pheno.getBirthYear() + ";" + +pheno.getPollinationDate() + ";" + sept1);
				}

			}

		}

		if (phenoOfInterest != null) { // frost for senec

			// System.out.println("FmDeciduousPhenologyRunner phenoFitlib()
			// year: "+cell.currentYear+" day: "+day+" cell: "+cell.getID()
			// +" L: "+cell.getCanopy().getLAI()+" leafIndex:
			// "+phenoOfInterest.getLeafIndex());

			// TO BE IMPROVED
			// Leaf fall is not managed with phenofit here, we do not
			// consider frost damage during autumn
			// if (day < LFALL) {// frost effect on senescence not simulated

			leafFrostDamage = phenoOfInterest.leafMemory.frostDamage;
			leafFrostHardiness = phenoOfInterest.leafMemory.frostHardiness;

			// } else {
			// leafFrostDamage = 0;
			// leafFrostHardiness = -20;
			// }

		}

		if (!firstYearOfSimulation && phenoOfInterest != null) {

			// Get leaf leaf dormancy break date if available
			if (leafDormancyBreakDate == 0 && phenoOfInterest.isSetLeafDormancyBreakDate())
				leafDormancyBreakDate = (int) phenoOfInterest.getLeafDormancyBreakDate();

			if (leafUnfoldingDate == 0 && phenoOfInterest.isSetLeafUnfoldingDate()) {
				leafUnfoldingDate = (int) phenoOfInterest.getLeafUnfoldingDate();
				BBDAY = leafUnfoldingDate;
				
				budburstAchieved = true; // fc-10.2.2020
			}

			// if (BBDAY == 0 && phenoOfInterest.isSetLeafUnfoldingDate())
			// BBDAY = (int) phenoOfInterest.getLeafUnfoldingDate();

			// Get the senescence date
			if (leafSenescenceDate == 0 && phenoOfInterest.isSetLeafSenescenceDate()) {
				leafSenescenceDate = (int) phenoOfInterest.getLeafSenescenceDate();
				LFALL = leafSenescenceDate;
			}
			// if (LFALL == 366 && phenoOfInterest.isSetLeafSenescenceDate())
			// LFALL = (int) phenoOfInterest.getLeafSenescenceDate();

			// fc+vj-4.9.2018 flower dormancy break date
			if (flowerDormancyBreakDate == 0 && phenoOfInterest.isSetFlowerDormancyBreakDate())
				flowerDormancyBreakDate = (int) phenoOfInterest.getFlowerDormancyBreakDate();

			// fc+vj-4.9.2018 flowering date
			if (floweringDate == 0 && phenoOfInterest.isSetFloweringDate())
				floweringDate = (int) phenoOfInterest.getFloweringDate();

			if (fruitGrowthInitDate == 0 && phenoOfInterest.isSetFruitGrowthInitDate())
				fruitGrowthInitDate = (int) phenoOfInterest.getFruitGrowthInitDate();

			if (fruitMaturationDate == 0 && phenoOfInterest.isSetFruitMaturationDate())
				fruitMaturationDate = (int) phenoOfInterest.getFruitMaturationDate();

			if (pollinationDate == 0 && phenoOfInterest.isSetPollinationDate())
				pollinationDate = (int) phenoOfInterest.getPollinationDate();

		}

		return phenoOfInterest;

	} // end-of-phenoFitlib

	/**
	 * Management of leaf index.
	 */
	// fc+ic-6.6.2017
	private void calculateLeafIndex(FmPhenology pheno) {

		// In Phenofit5, leafIndex is calculated before calling the
		// maturation function
		// leafIndex = frostEffect * senescenceEffect

		double frostEffect = 1 - pheno.leafMemory.frostDamage;
		pheno.setLeafIndex(frostEffect * pheno.leafMemory.senescenceEffect);

	}

	/**
	 * Phenology, Phelib method
	 */
	// fc-10.2.2020 capsis.lib.castanea was disconnected from the PHELIB option on 18.4.2015, see FmSettings 
//	protected FmPhenology phenoPhelib(int day, FmClimateDay climateDay, boolean firstYearOfSimulation)
//			throws Exception {
//
//		// Create a new pheno if 1 sept // fc+hd+nb-28.11.2016
//		int sept1 = FitlibClimate.get1September(cell.currentYear);
//
//		if (day == sept1) {
//			// fc+hd+vj+nb-18.4.2017
//			boolean deciduous = true;
//			FmPhenology phelibPheno = new FmPhenology(cell.currentYear, deciduous);
//			cell.addPhelibPheno(phelibPheno);
//
//		}
//
//		// Process current day for all phenos, search pheno carrying the
//		// unfolding date // fc+hd+nb-28.11.2016
//		// FmPhenology phenoOfInterest = null;
//		FmPhenology lastPheno = null;
//		FmPhenology phenoOfInterest = null;
//
//		for (FmPhenology pheno : cell.getPhelibPhenoList()) {
//
//			lastPheno = pheno;
//
//			if (pheno.getBirthYear() == cell.currentYear - 1)
//				phenoOfInterest = pheno; // will canew FmFitlibUniforc(t0, d, e,
//											// Fcrit,
//											// FmPhenology.FLOWER_MODE);rry
//											// unfolding date
//
//			settings.phelibConnector.leafUnfolding(cell, climateDay, climateDay.getYear(), day, pheno);
//			if (pheno.isSetLeafDormancyBreakDate() && leafDormancyBreakDate == 0)
//				leafDormancyBreakDate = (int) pheno.getLeafDormancyBreakDate();
//			if (pheno.isSetLeafUnfoldingDate() && leafUnfoldingDate == 0) {
//				leafUnfoldingDate = (int) pheno.getLeafUnfoldingDate();
//				BBDAY = leafUnfoldingDate;
//			}
//			// if (pheno.isSetLeafUnfoldingDate() && BBDAY == 0)
//			// BBDAY = (int) pheno.getLeafUnfoldingDate();
//
//			settings.phelibConnector.flowering(cell, climateDay, climateDay.getYear(), day, pheno);
//			if (pheno.isSetFlowerDormancyBreakDate() && flowerDormancyBreakDate == 0)
//				flowerDormancyBreakDate = (int) pheno.getFlowerDormancyBreakDate();
//			if (pheno.isSetFloweringDate() && floweringDate == 0)
//				floweringDate = (int) pheno.getFloweringDate();
//
//			settings.phelibConnector.leafSenescence(cell, climateDay, climateDay.getYear(), day, pheno);
//			if (pheno.isSetLeafSenescenceDate() && leafSenescenceDate == 0)
//				leafSenescenceDate = (int) pheno.getLeafSenescenceDate();
//
//			calculateLeafSenescenceEffect(cell, pheno, day);
//
//			cell.leafFrost.execute(cell, settings, climateDay, day, pheno);
//
//			calculateLeafIndex(pheno, cell.currentYear, day);
//
//			settings.phelibConnector.fruitMaturation(cell, climateDay, climateDay.getYear(), day, pheno);
//			if (pheno.isSetFruitGrowthInitDate() && fruitGrowthInitDate == 0)
//				fruitGrowthInitDate = (int) pheno.getFruitGrowthInitDate();
//			if (pheno.isSetFruitMaturationDate() && fruitMaturationDate == 0)
//				fruitMaturationDate = (int) pheno.getFruitMaturationDate();
//
//			// TODO calculateMaturationIndex(ip.phenologyConnector, p,
//			// pheno, year, d);
//
//			// TODO flowerFrost ()
//
//			// TODO calculateFruitIndex(ip.phenologyConnector, p, pheno,
//			// year, d, prevFrostDamage, pheno.flowerMemory);
//
//		}
//
//		if (phenoOfInterest != null) {
//			leafFrostDamage = phenoOfInterest.leafMemory.frostDamage;
//			leafFrostHardiness = phenoOfInterest.leafMemory.frostHardiness;
//		}
//
//
//		 //if (firstPheno != null) L= getLaiDaily(cell, settings, species,
//		 //firstPheno, Tmoy, day);
//
//		if (settings.output > 1 && lastPheno != null && phenoOfInterest != null) {
//			Log.println(settings.logPrefix + "phelib",
//					day + ";" + phenoOfInterest.leafMemory.frostDamage + ";" + lastPheno.leafMemory.frostDamage + ";"
//							+ phenoOfInterest.leafMemory.frostHardiness + ";" + lastPheno.leafMemory.frostHardiness
//							+ ";" + phenoOfInterest.leafMemory.state + ";" + lastPheno.leafMemory.state);
//		}
//
//		return phenoOfInterest;
//
//	} // end-of-phenoPhelib

	/** 
	 * capsis.lib.phenofit.fit2018 library connection, fc+vj-27.9.2018
	 */
	protected Fit2018Pheno pheno_Fit2018(int day, FmClimateDay climateDay, boolean firstYearOfSimulation)
			throws Exception {

		// day is expected in [1,366]

		// fc+som-3.12.2018 to be validated by HD
		// int fit2018PhenoYear = cell.currentYear + 1;
		// fc+hd+nb-10.12.2018
		int fit2018PhenoYear = cell.currentYear;

		Fit2018Date today = Fit2018Date.getInstance(fit2018PhenoYear, day);
		
		// fc-10.2.2020 Reset budburstAchieved on 1 January
		if (day == 1)
			budburstAchieved = false;

		Fit2018PhenoManager phenoManager = cell.getPhenoManager();
		phenoManager.manage(cell, climateDay, today);

		Fit2018Pheno pheno = cell.getPheno();

		// Write available results in instance variables before returning

		if (pheno != null) {

			if (leafSenescenceDate == 0) {
				leafFrostDamage = pheno.getLeafFrostDamage();
				leafFrostHardiness = pheno.getLeafFrostHardiness();
			}

			// Get leaf leaf dormancy break date if available
			if (leafDormancyBreakDate == 0 && pheno.getLeafDormancyBreakDate() != null)
				leafDormancyBreakDate = (int) pheno.getLeafDormancyBreakDate().getDayOfYear();

			// Get leaf unfolding date if available
			if (leafUnfoldingDate == 0 && pheno.getLeafUnfoldingDate() != null) {
				leafUnfoldingDate = (int) pheno.getLeafUnfoldingDate().getDayOfYear();
				BBDAY = leafUnfoldingDate;
				
				budburstAchieved = true; // fc-10.2.2020
			}

			// Get the senescence date
			if (leafSenescenceDate == 0 && pheno.getLeafSenescenceDate() != null) {
				leafSenescenceDate = (int) pheno.getLeafSenescenceDate().getDayOfYear();
				LFALL = leafSenescenceDate;
			}

			// fc+vj-4.9.2018 flower dormancy break date
			if (flowerDormancyBreakDate == 0 && pheno.getFlowerDormancyBreakDate() != null)
				flowerDormancyBreakDate = (int) pheno.getFlowerDormancyBreakDate().getDayOfYear();

			// fc+vj-4.9.2018 flowering date
			if (floweringDate == 0 && pheno.getFloweringDate() != null)
				floweringDate = (int) pheno.getFloweringDate().getDayOfYear();

			if (fruitGrowthInitDate == 0 && pheno.getFruitGrowthInitDate() != null)
				fruitGrowthInitDate = (int) pheno.getFruitGrowthInitDate().getDayOfYear();

			if (fruitMaturationDate == 0 && pheno.getFruitMaturationDate() != null)
				fruitMaturationDate = (int) pheno.getFruitMaturationDate().getDayOfYear();

			// fc+vj-27.9.2018 Standby
			// if (pollinationDate == 0 && pheno.isSetPollinationDate())
			// pollinationDate = (int) phenoOfInterest.getPollinationDate();

		}

		return pheno;
	}

	/**
	 * Effect of frost on leaf area index, returns leaf area index after frost
	 */
	protected double leafFrostEffect(FmFluxesResults yearlyResults, int day, double Tmin, double coefSecondFlush,
			double LMAXwithoutFrost, FmCanopy canopy) {

		// fc+hd-22.6.2018

		double LMAX = canopy.getLAImax();

		if (settings.phenoMode.equals(FmSettings.PHENO_CASTANEA)
				|| settings.phenoMode.equals(FmSettings.PHENO_FORCED)) {
					
			Log.println("On rentre dans 1");
			Log.println("Tmin: " + Tmin);
			Log.println("FHminfe: " + canopy.getFHminfe());
			Log.println("BBDAY: " + BBDAY);
			Log.println("DAYLMA: " + DAYLMA);

			if (Tmin < canopy.getFHminfe() && BBDAY > 0 && DAYLMA == 0 && species.decidu == 1) {
				// after budburst

				double L = cell.getCanopy().getLAI();

				double lateFrostNumber = yearlyResults.lateFrostNumber;
				yearlyResults.lateFrostNumber= lateFrostNumber + 1;
				leafFrostDamage = 1 - (Tmin - canopy.getFHminfe()) * settings.frostEffectCoef;
				memoryLeafFrostDamage = leafFrostDamage;
				diffLAIFrost = LwithoutFrost - L;
				dayOfLateFrost = day; // the last late forst of the year

			} else if(Tmin < canopy.getFHminfe() && species.decidu == 2) {
				
				double L = cell.getCanopy().getLAI();

				double lateFrostNumber = yearlyResults.lateFrostNumber;
				yearlyResults.lateFrostNumber= lateFrostNumber + 1;
				leafFrostDamage = 1 - (Tmin - canopy.getFHminfe()) * settings.frostEffectCoef;
				memoryLeafFrostDamage = leafFrostDamage;
				diffLAIFrost = LwithoutFrost - L;
				dayOfLateFrost = day; // the last late forst of the year
				
				Log.println("On rentre dans 2");
				
				
			}

		} else { // FmSettings.PHENO_FITLIB_* || FmSettings.PHENO_PHELIB

			if (leafFrostDamage > 0.01 && leafFrostDamage - memoryLeafFrostDamage > 0.01 && day < LFALL) {
				memoryLeafFrostDamage = leafFrostDamage;
				double lateFrostNumber = yearlyResults.lateFrostNumber;
				yearlyResults.lateFrostNumber= lateFrostNumber + 1;
				dayOfLateFrost = day; // the last late forst of the year

			}

		}

		LMAX = LMAXwithoutFrost * (1 - leafFrostDamage);
		LMAX = Math.max(0.1, LMAX); // residual LAI of 0.1
		diffLAIFrost = LMAXwithoutFrost - LMAX;


		if (leafFrostDamage > yearlyResults.maxFrostEffect &&  day < LFALL)
			yearlyResults.maxFrostEffect=  leafFrostDamage;

		if (settings.iFROST > 1 && BBDAY2 > 0)
			LMAXsecondFlush = Math.max(coefSecondFlush * LMAXwithoutFrost - LMAX, 0.);

		return LMAX;
	}

	protected void setLeafFrostNumber(FmFluxesResults yearlyResults, int day, double Tmin, FmCanopy canopy) {

		// hd-08.08.2019
		if (settings.phenoMode.equals(FmSettings.PHENO_CASTANEA)
				|| settings.phenoMode.equals(FmSettings.PHENO_FORCED)) {

			if (Tmin < canopy.getFHminfe() && BBDAY > 0 && DAYLMA == 0) {
				// after budburst

				double L = cell.getCanopy().getLAI();

				double lateFrostNumber = yearlyResults.lateFrostNumber;
				yearlyResults.lateFrostNumber= lateFrostNumber + 1;
				dayOfLateFrost = day; // the last late forst of the year

			} else if(Tmin < canopy.getFHminfe() && species.decidu == 2) {
				
				double L = cell.getCanopy().getLAI();

				double lateFrostNumber = yearlyResults.lateFrostNumber;
				yearlyResults.lateFrostNumber= lateFrostNumber + 1;
				dayOfLateFrost = day; // the last late forst of the year
				
			}

		} else { // FmSettings.PHENO_FITLIB_* || FmSettings.PHENO_PHELIB

			if (leafFrostDamage > 0.01 && leafFrostDamage - memoryLeafFrostDamage > 0.01 && day < LFALL) {
				memoryLeafFrostDamage = leafFrostDamage;
				double lateFrostNumber = yearlyResults.lateFrostNumber;
				yearlyResults.lateFrostNumber= lateFrostNumber + 1;
				dayOfLateFrost = day; // the last late frost of the year

			}

		}
	}

	/**
	 * Management of the leaf senescence effect.
	 */
	// fc+hd+vj-23.4.2018 The code below was previously in Fit5Model (Delpierre)

	protected void calculateLeafSenescence(FmCell cell, FmPhenology pheno, FmClimateDay climateDay, int day)
			throws Exception {

		// starts the day after unfolding date
		boolean unfoldingDatePassed = (int) pheno.getLeafUnfoldingDate() != FitlibPhenology.NOT_SET
				&& day > Math.ceil(pheno.getLeafUnfoldingDate());

		if (!unfoldingDatePassed)
			return;

		if (day == Math.ceil(pheno.getLeafUnfoldingDate() + 1))
			// Senescence starts: reset the memory
			pheno.leafMemory.resetDevelopmentStates();

		int year = cell.currentYear;
		pheno.setMode(FmPhenology.SENESCENCE_MODE);
		FitlibDailyMemory memory = pheno.leafMemory;

		int nbDays = FitlibClimate.getNbDays(year);
		int sept1 = FitlibClimate.get1September(year);
		int nbDays0 = FitlibClimate.getNbDays(year - 1);

		int prevPhase = memory.phase;
		double prevState = memory.state;

		int yesterday = FitlibClimate.getYesterday(year, day);
		int fit4Today = pheno.getPhenofit4Date(year, day);

		double Tmoy = climateDay.getDailyAverageTemperature();

		if (prevPhase <= 3) {

			// Is phase 3 over ?
			if (prevPhase == 3 && prevState >= 1) { // fc+ic-15.6.2017

				// fc+ic-15.6.2017 one day too late, moved below
				// pheno.setDate1(year, yesterday);

				// fc+ic-15.6.2017 introduced phase 4
				memory.phase = 4;

				memory.resetDevelopmentStates();
				prevState = memory.state;

			} else {

				// Process phase 3
				memory.phase = 3;

				double dayLength = climateDay.getDayLength();

				double Sen = 0;
				if (fit4Today > FitlibConstants.SUMMER_SOLSTICE && dayLength < senescence_Pb && Tmoy < senescence_Tb)
					Sen = Math.pow(senescence_Tb - Tmoy, senescence_alpha)
							* Math.pow(dayLength / senescence_Pb, senescence_beta) + prevState * senescence_Scrit;

				memory.state = Sen / senescence_Scrit;

				// fc+ic-15.6.2017 date is set at end of phase
				if (memory.state >= 1 && !pheno.isSetLeafSenescenceDate())
					pheno.setDate1(year, day);

			}

		}

		// fc+ic-15.6.2017 new phase 4 to continue computing the state,
		// (reset to zero at phase 4 start time to comply with phelib)
		if (memory.phase == 4 || prevPhase == 4) {

			double dayLength = climateDay.getDayLength();

			double Sen = 0;
			if (fit4Today > FitlibConstants.SUMMER_SOLSTICE && dayLength < senescence_Pb && Tmoy < senescence_Tb)
				Sen = Math.pow(senescence_Tb - Tmoy, senescence_alpha)
						* Math.pow(dayLength / senescence_Pb, senescence_beta) + prevState * senescence_Scrit;

			memory.state = Sen / senescence_Scrit;

		}

	}

	/**
	 * Management of the leaf senescence effect.
	 */
	// fc+hd+vj-23.4.2018 The code below was previously in Fit5Model (Delpierre)
	protected void calculateLeafSenescenceEffect(FmCell cell, FmPhenology pheno, int d) throws Exception {

		pheno.leafMemory.senescenceEffect = 1;

		if ((int) pheno.getLeafUnfoldingDate() != FitlibPhenology.NOT_SET
				&& d > Math.ceil(pheno.getLeafUnfoldingDate())) { // fc+ic-10.5.2017

			double sigmaSen = cell.getSpecies().getSenescenceSigma();
			double Scrit = 1;
			double Sen = 0;

			boolean afterSenescenceDate = pheno.isSetLeafSenescenceDate() && d > pheno.getLeafSenescenceDate();

			if (!afterSenescenceDate) {
				Sen = pheno.leafMemory.state * Scrit;
			} else {
				Sen = Scrit + pheno.leafMemory.state * Scrit;
			}

			// fc+ic+jg-30.8.2016 senescence may affect leafIndex
			if ((Sen >= Scrit - 3 * sigmaSen) && (pheno.isDeciduous())) {

				double effect = 1 - ((Sen - (Scrit - 3 * sigmaSen)) / (6 * sigmaSen));
				effect = Math.max(effect, 0);
				pheno.leafMemory.senescenceEffect = effect;

			} else {

				pheno.leafMemory.senescenceEffect = 1; // no effect if
														// sempervirens or
														// prior senescence
			}

		}

	}

	/**
	 * Management of leaf index.
	 */
	// fc+hd+vj-23.4.2018 The code below was previously in Fit5Model

	protected void calculateLeafIndex(FmPhenology pheno, int year, int day) {

		// In Phenofit5, leafIndex is calculated before calling the
		// maturation function
		// leafIndex = frostEffect * senescenceEffect // fc+ic-31.8.2016

		double frostEffect = 1 - pheno.leafMemory.frostDamage;
		pheno.setLeafIndex(frostEffect * pheno.leafMemory.senescenceEffect);

		// pheno.setLeafIndex(frostEffect); // pb with
		// pheno.leafMemory.senescenceEffect

		// System.out.println("FmPhenologyRunner.calculateLeafIndex () year/day:
		// " +year+"/"+day+" frostDamage: " + pheno.leafMemory.frostDamage
		// +" frostEffect: " + frostEffect + " senescenceEffect: " +
		// pheno.leafMemory.senescenceEffect
		// + " leafIndex: " + pheno.getLeafIndex());

	}

	/*
	 * public double getLaiDaily(FmCell cell, FmSettings settings, FmSpecies
	 * species, FmPhenology pheno, double Tmoy, int day) throws Exception {
	 *
	 * double laiDaily = 0; double L= cell.getCanopy().getLAI(); double LMAX=
	 * cell.getCanopy().getLAImax();
	 *
	 * if (cell.species.decidu == 1) {
	 *
	 * if (!pheno.isSetLeafUnfoldingDate()) { laiDaily = 0; // before leaf
	 * UnforldingDAte, lai=0
	 *
	 * } else if (!pheno.isSetLeafSenescenceDate()) { HSUMLA = HSUMLA + Tmoy;
	 * TLMA = TLMA + Tmoy; if (TLMA >= species.HSUMLMA && DAYLMA == 0) { // day
	 * when leaf // is mature DAYLMA = day; } double leafGrowth = Math.min(1.,
	 * HSUMLA / species.HSUMFL);
	 *
	 * // LMAXN : date where maximum LAI // is obtained if (day < LMAXN &&
	 * (Math.abs(LMAX - L) < settings.eps || L > LMAX)) { LMAXN = day; } if
	 * (settings.iFROST > 1 && day > BBDAY2) { if (day < LMAXN2 &&
	 * (Math.abs(LMAX + LMAXsecondFlush - L) < settings.eps || L > LMAX +
	 * LMAXsecondFlush)) { LMAXN2 = day; } }
	 *
	 * if (day > species.NSTART3 && Tmoy < species.TBASEC) { HSUMLFAL = HSUMLFAL
	 * + (species.TBASEC - Tmoy); }
	 *
	 * if (HSUMLFAL > cell.getWood().getWoodStop() && day < dayOfWoodStop) {
	 * dayOfWoodStop = day; }
	 *
	 * if (day < LFALL) { laiDaily = cell.getCanopy().getLAImax() *
	 * pheno.leafIndex*leafGrowth; // between } else { laiDaily =
	 * cell.getCanopy().getLAImax() * pheno.leafMemory.senescenceEffect; }
	 *
	 * } else { laiDaily = 0; // after leafSenescenceDate lai=0
	 *
	 * }
	 *
	 * } else { laiDaily = LMAX; // if not deciduous // lai=laiMax all year //
	 * long // always }
	 *
	 * return laiDaily;
	 *
	 * }
	 */

	public int getLeafDormancyBreakDate() {
		return leafDormancyBreakDate;
	}

	public int getLeafUnfoldingDate() { // = BBDAY (fc-5.9.2018)
		return leafUnfoldingDate;
	}

	public int getLeafSenescenceDate() {
		return leafSenescenceDate;
	}

	public int getFlowerDormancyBreakDate() {
		return flowerDormancyBreakDate;
	}

	public int getFloweringDate() {
		return floweringDate;
	}

	public int getFruitGrowthInitDate() {
		return fruitGrowthInitDate;
	}

	public int getFruitMaturationDate() {
		return fruitMaturationDate;
	}

	public int getPollinationDate() {
		return pollinationDate;
	}

	public int getBBDAY() {
		return BBDAY;
	}

	public int getLMAXN() {
		return LMAXN;
	}

	public int getLFALL() {
		return LFALL;
	}

	public double getLFALLH() {
		return LFALLH;
	}

	public int getLEND() {
		return LEND;
	}

	public int getDAYLMA() {
		return DAYLMA;
	}

	public double getTSUM() {
		return TSUM;
	}

	public double getTLMA() {
		return TLMA;
	}

	public double getHSUMLA() {
		return HSUMLA;
	}

	public double getHSUMLFAL() {
		return HSUMLFAL;
	}

	public double getTSUMFALD() {
		return TSUMFALD;
	}

	public double getDayOfLateFrost() {
		return dayOfLateFrost;
	}

	public double getLeafFrostDamage() {
		return leafFrostDamage;
	}

	public void setLeafDormancyBreakDay(int leafDormancyBreakDate) {
		this.leafDormancyBreakDate = leafDormancyBreakDate;
	}

	public void setBBDAY(int v) {
		BBDAY = v;
	}

	public void setFlowerDormancyBreakDay(int flowerDormancyBreakDate) {
		this.flowerDormancyBreakDate = flowerDormancyBreakDate;
	}

	public void setFloweringDay(int floweringDate) {
		this.floweringDate = floweringDate;
	}

	public void setFruitGrowthInitDate(int fruitGrowthInitDate) {
		this.fruitGrowthInitDate = fruitGrowthInitDate;
	}

	public void setFruitMaturationDay(int fruitMaturationDate) {
		this.fruitMaturationDate = fruitMaturationDate;
	}

	public void setLeafSenescenceDate(int leafSenescenceDate) {
		this.leafSenescenceDate = leafSenescenceDate;
	}

	public void setLMAXN(int v) {
		LMAXN = v;
	}

	public void setLFALL(int v) {
		LFALL = v;
	}

	public void setLFALLH(double v) {
		LFALLH = v;
	}

	public void setLEND(int v) {
		LEND = v;
	}

	public void setDAYLMA(int v) {
		DAYLMA = v;
	}

	public void setTSUM(double v) {
		TSUM = v;
	}

	public void setTLMA(double v) {
		TLMA = v;
	}

	public void setHSUMLA(double v) {
		HSUMLA = v;
	}

	public void setHSUMLFAL(double v) {
		HSUMLFAL = v;
	}

	public void setTSUMFALD(double v) {
		TSUMFALD = v;
	}

	public void setDayOfWoodStop(double v) {
		dayOfWoodStop = v;
	}

	public void setDayOfEndFall(double v) {
		dayOfEndFall = v;
	}

	public void setDayOfBeginFall(double v) {
		dayOfBeginFall = v;
	}

	public void setDayOfLateFrost(double v) {
		dayOfLateFrost = v;
	}

} // end of class
