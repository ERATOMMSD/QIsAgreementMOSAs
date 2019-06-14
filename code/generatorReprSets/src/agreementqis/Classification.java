package agreementqis;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

public class Classification {
	private static Map<QI, Set<Category>> category;

	static {
		category = new HashMap<QI, Set<Category>>();
		for (QI qi : QI.values()) {
			category.put(qi, new HashSet<Category>());
		}

		// Classification taken from
		// Shuai Wang, Shaukat Ali, Tao Yue, Yan Li, and Marius Liaaen.
		// A practical guide to select quality indicators for assessing pareto-based
		// search algorithms in search-based software engineering.
		// In Proceedings of the 38th International Conference on Software Engineering
		// (ICSE '16)

		// A similar classification is given in:
		//Miha Ravber, Marjan Mernik, Matej Crepinsek.
		//The impact of Quality Indicators on the rating of Multi-objective Evolutionary Algorithms,
		//In Applied Soft Computing, Volume 55, 2017, Pages 265-275

		category.get(QI.GD).add(Category.CONVERGENCE);
		category.get(QI.ED).add(Category.CONVERGENCE);
		category.get(QI.EP).add(Category.CONVERGENCE);
		category.get(QI.GS).add(Category.DIVERSITY);
		category.get(QI.PFS).add(Category.DIVERSITY);
		category.get(QI.C).add(Category.COVERAGE);

		// In the paper, IGD and HV are in the "Combination" category, that contains
		// those QIs that combine convergence and diversity
		category.get(QI.IGD).add(Category.CONVERGENCE);
		category.get(QI.IGD).add(Category.DIVERSITY);
		category.get(QI.HV).add(Category.CONVERGENCE);
		category.get(QI.HV).add(Category.DIVERSITY);
	}

	public static void main(String[] args) throws IOException {
		agreeCatAll();
		agreeCatAlgs();
	}

	private static void agreeCatAll() throws IOException {
		System.out.println("QI1\tQI2\tagree\tsameCat\tcatAsAgree");
		DefaultDirectedGraph<QI, DefaultEdge> agreement = RepSetsGenerator
				.buildGraphFromFile("../../statisticalTests/results/rq1.txt", null, null);
		QI[] qis = agreement.vertexSet().toArray(new QI[agreement.vertexSet().size()]);
		for (int i = 0; i < qis.length - 1; i++) {
			QI qiA = qis[i];
			for (int j = i + 1; j < qis.length; j++) {
				QI qiB = qis[j];
				System.out.print(qiA + "\t" + qiB + "\t");
				boolean agree = agreement.containsEdge(qiA, qiB);
				System.out.print(agree);
				Set<Category> catA = new HashSet<>(category.get(qiA));
				Set<Category> catB = new HashSet<>(category.get(qiB));
				catA.retainAll(catB);
				boolean sameCat = catA.size() > 0;
				System.out.print("\t" + sameCat);
				boolean catAsAgree = (sameCat == agree);
				System.out.println("\t" + catAsAgree);
			}
		}
	}

	private static void agreeCatAlgs() throws IOException {
		System.out.println("QI1\tQI2\tAlg1\tAlg2\tagree\tsameCat\tcatAsAgree");
		MOSA[] mosas = MOSA.values();
		for (int ai = 0; ai < mosas.length; ai++) {
			MOSA alg1 = mosas[ai];
			for (int aj = ai + 1; aj < mosas.length; aj++) {
				MOSA alg2 = mosas[aj];
				DefaultDirectedGraph<QI, DefaultEdge> agreement = RepSetsGenerator
						.buildGraphFromFile("../../statisticalTests/results/rq3_2.txt", alg1, alg2);
				QI[] qis = agreement.vertexSet().toArray(new QI[agreement.vertexSet().size()]);
				for (int i = 0; i < qis.length - 1; i++) {
					QI qiA = qis[i];
					for (int j = i + 1; j < qis.length; j++) {
						QI qiB = qis[j];
						System.out.print(qiA + "\t" + qiB + "\t" + alg1 + "\t" + alg2 + "\t");
						boolean agree = agreement.containsEdge(qiA, qiB);
						System.out.print(agree);
						assert category.get(qiA) != null : qiA;
						assert category.get(qiB) != null : qiB;
						Set<Category> catA = new HashSet<>(category.get(qiA));
						Set<Category> catB = new HashSet<>(category.get(qiB));
						catA.retainAll(catB);
						boolean sameCat = catA.size() > 0;
						System.out.print("\t" + sameCat);
						boolean catAsAgree = (sameCat == agree);
						System.out.println("\t" + catAsAgree);
					}
				}
			}
		}
	}

}
