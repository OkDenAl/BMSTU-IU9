package com.company;

public class Main {

    public static void main(String[] args) {
        //Создание объекта класса и проверка работоспособности
        NODList a =new NODList(5);
        System.out.println("Проитерировано по НОДам:");
        for (Object n : a){
            System.out.printf("%d  ",n);
        }
        System.out.println("\nИсходная последовательность была:");
        a.Printer();

        //Добавление элемента и реакция программы на это
        if (a.addElem(3,56) == 1) {
            System.out.println("\nПроитерировано по НОДам:");
            for (Object n : a) {
                System.out.printf("%d  ", n);
            }
            System.out.println("\nИсходная последовательность была:");
            a.Printer();
        }

        //Удаление элемента и реакция программы на это
        if (a.deleteElem(2) == 1) {
            System.out.println("\nПроитерировано по НОДам:");
            for (Object n : a) {
                System.out.printf("%d  ", n);
            }
            System.out.println("\nИсходная последовательность была:");
            a.Printer();
        }
    }
}
