package com.company;

public class Main {
    public static void main(String[] args) {
        TemplateNumbers nums = new TemplateNumbers();
        nums.addNumber(1235);
        nums.addNumber(230);
        nums.addNumber(62357);
        nums.addNumber(162300);
        nums.addNumber(12430);
        nums.addNumber(1234);
        nums.templateMatching("??2?3?").sorted(new NumberComparatorMinToMax()).forEach(System.out::println);
        System.out.printf("%s ","Максимальное число последовательности: ");
        System.out.println(nums.getMaxNum().get());
    }
}

