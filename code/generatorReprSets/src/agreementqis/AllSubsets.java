package agreementqis;

import java.util.ArrayList;
import java.util.List;

public class AllSubsets<E> {
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