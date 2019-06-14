package agreementqis;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

public class RepSetsGenerator {

	public static void main(String[] args) throws IOException {
		computeQImaxAgreement();// for SR
		computeMinimalSetAllQIs();// for MRS
	}

	/**
	 * This is for SR
	 * 
	 * @throws IOException
	 */
	private static void computeQImaxAgreement() throws IOException {
		computeQImaxAgreement("../../statisticalTests/results/rq1.txt", null, null);
		for (MOSA alg1 : MOSA.values()) {
			computeQImaxAgreement("../../statisticalTests/results/rq3_1.txt", alg1, null);
		}
		MOSA[] mosas = MOSA.values();
		for (int i = 0; i < mosas.length; i++) {
			MOSA alg1 = mosas[i];
			for (int j = i + 1; j < mosas.length; j++) {
				MOSA alg2 = mosas[j];
				computeQImaxAgreement("../../statisticalTests/results/rq3_2.txt", alg1, alg2);
			}
		}
	}

	/**
	 * This is for SR
	 * 
	 * @throws IOException
	 */
	private static void computeQImaxAgreement(String path, MOSA alg1, MOSA alg2) throws IOException {
		System.out.println("SR - " + (alg1 == null ? "ALL" : (alg2 == null ? alg1 : alg1 + "-" + alg2))
				+ " - QI -> [QI1,...,QIk], AVG Kappa among QI and QI1,...,QIk (average strength of the agreement), strength of the agreement");
		DefaultDirectedGraph<QI, DefaultEdge> graph = RepSetsGenerator.buildGraphFromFile(path, alg1, alg2);
		Map<String, String> kappa = RepSetsGenerator.getKappa(path, alg1, alg2);

		Map<QI, Set<QI>> covers = new HashMap<>();
		Map<QI, Double> avgStrengthAgreement = new HashMap<>();
		Map<QI, Double> strengthAgreement = new HashMap<>();
		for (QI v : graph.vertexSet()) {
			Set<QI> covered = graph.outgoingEdgesOf(v).stream().map(x -> graph.getEdgeTarget(x))
					.collect(Collectors.toSet());
			covers.put(v, covered);
			double k = 0;
			int numPairs = 0;
			for (QI c : covered) {
				k += Double.parseDouble(kappa.get(v + "_" + c));
				numPairs++;
			}
			avgStrengthAgreement.put(v, (numPairs > 0 ? k / numPairs : 0));
			strengthAgreement.put(v, covered.size() + (k / (numPairs + 1)));
		}
		covers.entrySet().stream().sorted(new Comparator<Map.Entry<QI, Set<QI>>>() {
			@Override
			public int compare(Map.Entry<QI, Set<QI>> o1, Map.Entry<QI, Set<QI>> o2) {
				int diffSize = o2.getValue().size() - o1.getValue().size();
				if (diffSize != 0) {
					return diffSize;
				} else {
					double diffAvgStrengthAgreement = avgStrengthAgreement.get(o2.getKey())
							- avgStrengthAgreement.get(o1.getKey());
					return (diffAvgStrengthAgreement > 0 ? 1 : (diffAvgStrengthAgreement < 0 ? -1 : 0));
				}
			}
		}).forEach(k -> System.out.println(k.getKey() + "->" + k.getValue() + ", "
				+ avgStrengthAgreement.get(k.getKey()) + ", " + strengthAgreement.get(k.getKey())));
		System.out.println();
	}

	/**
	 * This is for MRS
	 * 
	 * @throws IOException
	 */
	private static void computeMinimalSetAllQIs() throws IOException {
		print(computeMinimalSetAllQIs("../../statisticalTests/results/rq1.txt", null, null), null, null);
		for (MOSA alg1 : MOSA.values()) {
			print(computeMinimalSetAllQIs("../../statisticalTests/results/rq3_1.txt", alg1, null), alg1, null);
		}
		MOSA[] mosas = MOSA.values();
		for (int i = 0; i < mosas.length; i++) {
			MOSA alg1 = mosas[i];
			for (int j = i + 1; j < mosas.length; j++) {
				MOSA alg2 = mosas[j];
				print(computeMinimalSetAllQIs("../../statisticalTests/results/rq3_2.txt", alg1, alg2), alg1, alg2);
			}
		}
	}

	/**
	 * This is for MRS
	 * 
	 * @return
	 * 
	 * @throws IOException
	 */
	static Map<String, Double> computeMinimalSetAllQIs(String path, MOSA alg1, MOSA alg2) throws IOException {
		DefaultDirectedGraph<QI, DefaultEdge> graph = RepSetsGenerator.buildGraphFromFile(path, alg1, alg2);
		Map<String, String> kappa = RepSetsGenerator.getKappa(path, alg1, alg2);
		Map<QI, Set<QI>> covers = new HashMap<>();
		for (QI v : graph.vertexSet()) {
			Set<QI> covered = graph.outgoingEdgesOf(v).stream().map(x -> graph.getEdgeTarget(x))
					.collect(Collectors.toSet());
			covers.put(v, covered);
		}

		List<QI> set = new ArrayList<>(graph.vertexSet());
		AllSubsets<QI> it = new AllSubsets<QI>(set);
		it.next();
		int maxSize = 9;
		Set<String> suitableLists = null;
		while (it.hasNext()) {
			List<QI> next = it.next();
			Collections.sort(next);
			Set<QI> cov = new HashSet<>();
			for (QI s : next) {
				cov.addAll(covers.get(s));
			}
			cov.addAll(next);
			if (cov.size() == 8) {
				if (next.size() < maxSize) {
					suitableLists = new HashSet<>();
					maxSize = next.size();
				}
				if (next.size() == maxSize) {
					suitableLists.add(next.toString());
				}
			}
		}
		Map<String, Double> result = new HashMap<>();
		for (String s : suitableLists) {
			String[] a = s.substring(1, s.length() - 1).split(", ");
			double avgKappa = 0;
			int numPairs = 0;
			for (int i = 0; i < a.length; i++) {
				String aa = a[i];
				for (int j = i + 1; j < a.length; j++) {
					String aaa = a[j];
					if (!aaa.equals(aa)) {
						avgKappa += Double.parseDouble(kappa.get(aa + "_" + aaa));
						numPairs++;
					}
				}
			}
			// System.err.println(numPairs);
			result.put(s, (numPairs > 0 ? (avgKappa / numPairs) : 1));
		}
		return result;
	}

	static DefaultDirectedGraph<QI, DefaultEdge> buildGraphFromFile(String path, MOSA alg1, MOSA alg2)
			throws IOException {
		DefaultDirectedGraph<QI, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
		// Set<String> vertexes = new HashSet<>();
		List<String> lines = Files.readAllLines(Paths.get(path));
		String[] header = lines.get(0).split("\t");
		int indexQI1 = -1;
		int indexQI2 = -1;
		int indexBowkerP = -1;
		int indexkTestEst = -1;
		int indexAlg = -1;
		int indexPairAlgs = -1;
		for (int i = 0; i < header.length; i++) {
			if (header[i].equals("QI1")) {
				indexQI1 = i;
			} else if (header[i].equals("QI2")) {
				indexQI2 = i;
			} else if (header[i].equals("bowkerP")) {
				indexBowkerP = i;
			} else if (header[i].equals("kTestEst")) {
				indexkTestEst = i;
			} else if (header[i].equals("Alg")) {
				indexAlg = i;
			} else if (header[i].equals("pairAlgs")) {
				indexPairAlgs = i;
			}
		}
		for (int i = 1; i < lines.size(); i++) {
			String[] l = lines.get(i).split("\t");
			if (indexAlg != -1 && !l[indexAlg].equals(alg1.name())) {
				continue;
			}
			if (indexPairAlgs != -1 && !l[indexPairAlgs].equals(alg1.name() + "_" + alg2.name())
					&& !l[indexPairAlgs].equals(alg2.name() + "_" + alg1.name())) {
				continue;
			}
			QI QI1 = QI.valueOf(l[indexQI1]);
			QI QI2 = QI.valueOf(l[indexQI2]);
			graph.addVertex(QI1);
			graph.addVertex(QI2);
			double kappa = Double.parseDouble(l[indexkTestEst]);
			if (!l[indexBowkerP].equals("NA") && indexPairAlgs == -1) {
				double bowker = Double.parseDouble(l[indexBowkerP]);
				if (kappa >= 0.4 && bowker >= 0.05) {
					graph.addEdge(QI1, QI2);
					graph.addEdge(QI2, QI1);
				}
			} else {
				if (kappa >= 0.4) {
					graph.addEdge(QI1, QI2);
					graph.addEdge(QI2, QI1);
				}
			}
		}
		return graph;
	}

	private static Map<String, String> getKappa(String path, MOSA alg1, MOSA alg2) throws IOException {
		Map<String, String> temp = new HashMap<>();
		List<String> lines = Files.readAllLines(Paths.get(path));
		String[] header = lines.get(0).split("\t");
		int indexQI1 = -1;
		int indexQI2 = -1;
		int indexkTestEst = -1;
		int indexAlg = -1;
		int indexPairAlgs = -1;
		for (int i = 0; i < header.length; i++) {
			if (header[i].equals("QI1")) {
				indexQI1 = i;
			} else if (header[i].equals("QI2")) {
				indexQI2 = i;
			} else if (header[i].equals("kTestEst")) {
				indexkTestEst = i;
			} else if (header[i].equals("Alg")) {
				indexAlg = i;
			} else if (header[i].equals("PairAlgs")) {
				indexPairAlgs = i;
			}
		}
		for (int i = 1; i < lines.size(); i++) {
			String[] l = lines.get(i).split("\t");
			if (indexAlg != -1 && !l[indexAlg].equals(alg1.name())) {
				continue;
			}
			if (indexPairAlgs != -1 && !l[indexPairAlgs].equals(alg1.name() + "_" + alg2.name())
					&& !l[indexPairAlgs].equals(alg2.name() + "_" + alg1.name())) {
				continue;
			}
			String kappa = l[indexkTestEst];
			temp.put(l[indexQI1] + "_" + l[indexQI2], kappa);
			temp.put(l[indexQI2] + "_" + l[indexQI1], kappa);
		}
		return temp;
	}

	private static void print(Map<String, Double> computeMinimalSetAllQIs, MOSA alg1, MOSA alg2) {
		System.out.println("MRS - " + (alg1 == null ? "ALL" : (alg2 == null ? alg1 : alg1 + "-" + alg2))
				+ " - [QI1, ...,QIk], AVG Kappa among the couples of QIs");
		computeMinimalSetAllQIs.entrySet().stream().sorted(new Comparator<Entry<String, Double>>() {
			@Override
			public int compare(Entry<String, Double> o1, Entry<String, Double> o2) {
				double diff = o2.getValue() - o1.getValue();
				return (diff > 0 ? -1 : (diff < 0 ? 1 : 0));
			}
		}).forEach(k -> System.out.println(k.getKey() + ", " + k.getValue()));
		System.out.println();
	}

	static String min(Map<String, Double> computeMinimalSetAllQIs) {
		return computeMinimalSetAllQIs.entrySet().stream().sorted(new Comparator<Entry<String, Double>>() {
			@Override
			public int compare(Entry<String, Double> o1, Entry<String, Double> o2) {
				double diff = o2.getValue() - o1.getValue();
				return (diff > 0 ? -1 : (diff < 0 ? 1 : 0));
			}
		}).findFirst().get().getKey();
	}
}

class AllSubsets<E> {
	private final List<E> set;
	private final int max;
	private int index;

	public AllSubsets(List<E> originalList) {
		set = originalList;
		max = (1 << set.size());
		index = 0;
	}

	public boolean hasNext() {
		return index < max;
	}

	public List<E> next() {
		List<E> newSet = new ArrayList<E>();
		int flag = 1;
		for (E element : set) {
			if ((index & flag) != 0) {
				newSet.add(element);
			}
			flag <<= 1;
		}
		++index;
		return newSet;
	}
}
