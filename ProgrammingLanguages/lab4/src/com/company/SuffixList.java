package com.company;

import java.util.Iterator;

public class SuffixList implements Iterable {
private StringBuilder s;

public SuffixList ( StringBuilder s ) { this.s = s; }

public Iterator iterator() { return new SuffixIterator(); }

private class SuffixIterator implements Iterator {
    private int pos;

    public SuffixIterator () { pos = 0 ; }

    public boolean hasNext () { return pos < s.length(); }

    public String next () {
        return s.substring(pos++,s.length());
    }
}
	}

