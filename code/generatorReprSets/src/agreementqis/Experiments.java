package agreementqis;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Experiments {

	public static void main(String[] args) throws IOException {
		experiment("../../statisticalTests/results/rq1.txt", null, null);
		for (MOSA alg1 : MOSA.values()) {
			experiment("../../statisticalTests/results/rq3_1.txt", alg1, null);
		}
		for (MOSA alg1 : MOSA.values()) {
			for (MOSA alg2 : MOSA.values()) {
				if(alg1 != alg2) {
					experiment("../../statisticalTests/results/rq3_2.txt", alg1, alg2);
				}
			}
		}
	}

	private static void experiment(String path, MOSA alg1, MOSA alg2) throws IOException {
		String result = RepSetsGenerator.min(RepSetsGenerator.computeMinimalSetAllQIs(path, alg1, alg2));
		List<String> indicators = Arrays.asList(result.substring(1, result.length() - 1).split(", "));
		List<Integer> indicatorIndexes = new ArrayList<>();
		List<String> lines = Files.readAllLines(Paths.get("../../statisticalTests/inputData/inputData.csv"));
		String[] headers = lines.get(0).split(";");
		for (int i = 0; i < headers.length; i++) {
			String field = headers[i];
			if (indicators.contains(field)) {
				indicatorIndexes.add(i);
			}
		}
		int numExperiments = 0;
		int goToLiterature = 0;
		for (int i = 1; i < lines.size(); i++) {
			String[] line = lines.get(i).split(";");
			Map<String, Integer> evaluations = new HashMap<>();
			evaluations.put("A", new Integer(0));
			evaluations.put("B", new Integer(0));
			evaluations.put("ND", new Integer(0));
			for (int j = 0; j < line.length; j++) {
				if (indicatorIndexes.contains(j)) {
					String field = line[j];
					evaluations.put(field, evaluations.get(field) + 1);
				}
			}
			Integer numA = evaluations.get("A");
			Integer numB = evaluations.get("B");
			Integer numND = evaluations.get("ND");
			if (((numA > numB) && (numA > numND)) || ((numB > numA) && (numB > numND))
					|| ((numND > numA) && (numND > numB))) {

			} else {
				goToLiterature++;
			}
			numExperiments++;
		}
		System.out.print((alg1 == null ? "ALL" : alg1) + ": ");
		System.out.println((double) goToLiterature * 100 / numExperiments);
	}

}
