package com.company;

import java.util.ArrayList;
import java.util.Optional;
import java.util.stream.Stream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TemplateNumbers {
    private int n;
    private ArrayList<Integer> numbers;
    private ArrayList<Integer> isTemplate;
    TemplateNumbers(){
        numbers= new ArrayList<Integer>();
        n=0;
    }
    public void addNumber(int num){
        numbers.add(num);
        n++;
    }
    private boolean isTemplateMatch(int num, String template){
        String helper=""+num;
        if (helper.length()>template.length()){
            return false;
        }
        template=template.replace("?","\\d{0,1}");
        Pattern r = Pattern.compile(template);
        Matcher m = r.matcher(helper);
        if (m.find()){
            return helper.equals(helper.substring(m.start(),m.end()));
        }
        return false;
    }
    public Stream<Integer> templateMatching(String template){
        isTemplate = new ArrayList<Integer>();
        numbers.stream()
                .filter(x-> isTemplateMatch(x,template))
                .forEach(x -> isTemplate.add(x));
        return isTemplate.stream();
    }
    public Optional<Integer> getMaxNum() {
        Optional<Integer> res = Optional.empty();
        Optional<Integer> tmp = isTemplate.stream().sorted(new NumberComparatorMaxToMin()).findFirst();
        if (tmp.isPresent()) {
            res = Optional.ofNullable(tmp.get());
        }
        return res;
    }
}
