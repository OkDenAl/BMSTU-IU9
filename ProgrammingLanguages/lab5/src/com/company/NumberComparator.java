package com.company;
import java.util.*;
class NumberComparatorMinToMax implements Comparator<Integer> {
    public int compare(Integer a, Integer b) {
        if (a.toString().length() > b.toString().length()) { return 1; }
        if (a.toString().length() == b.toString().length()) { return 0; }
        return -1;
    }
}
class NumberComparatorMaxToMin implements Comparator<Integer> {
    public int compare(Integer a, Integer b) {
        if (a > b) { return -1; }
        if (a == b) { return 0; }
        return 1;
    }
}
