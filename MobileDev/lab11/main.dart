import 'dart:math';

import 'package:flutter/material.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Двоичное умножение',
      home: MultiplicationPage(),
    );
  }
}

class MultiplicationPage extends StatefulWidget {
  @override
  _MultiplicationPageState createState() => _MultiplicationPageState();
}

class _MultiplicationPageState extends State<MultiplicationPage> {
  String number1 = '';
  String number2 = '';
  String binary1 = '';
  String binary2 = '';
  String result_kar = '';
  String result_common = '';
  String errorMessage = '';
  String kar_t = '';
  String comm_t = '';

  void multiply() {
    try {
      int num1 = int.parse(number1);
      int num2 = int.parse(number2);

      // Проверка длины двоичного представления
      if (num1.toRadixString(2).length % 2 != 0) {
        throw Exception('Длина двоичного представления первого числа должна быть четной');
      }

      if (num2.toRadixString(2).length % 2 != 0) {
        throw Exception('Длина двоичного представления второго числа должна быть четной');
      }

      // Умножение методом Карацубы
      Stopwatch stopwatch = new Stopwatch()..start();
      result_kar = karatsuba(num1, num2).toString();
      kar_t = stopwatch.elapsed.toString();
      stopwatch = new Stopwatch()..start();
      result_common = naiveMultiplication(num1,num2).toString();
      comm_t = stopwatch.elapsed.toString();
      setState(() {
        binary1 = num1.toRadixString(2);
        binary2 = num2.toRadixString(2);
        errorMessage = '';
      });
    } catch (e) {
      setState(() {
        errorMessage = e.toString();
        result_kar = '';
      });
    }
  }

  int karatsuba(int x, int y) {
    if (x < (1 << 32) || y < (1 << 32)) {
      return naiveMultiplication(x, y);
    }

    int n = (x.bitLength > y.bitLength ? x.bitLength : y.bitLength);
    int half = n ~/ 2;

    int highX = x >> half;
    int lowX = x & ((1 << half) - 1);
    int highY = y >> half;
    int lowY = y & ((1 << half) - 1);

    int z0 = karatsuba(lowX, lowY);
    int z1 = karatsuba((lowX + highX), (lowY + highY));
    int z2 = karatsuba(highX, highY);

    return (z2 << (2 * half)) + ((z1 - z2 - z0) << half) + z0;
  }

  int naiveMultiplication(int x, int y) {
    List<int> xDigits = x.toRadixString(2).split('').map(int.parse).toList().reversed.toList();
    List<int> yDigits = y.toRadixString(2).split('').map(int.parse).toList().reversed.toList();

    int result = 0;
    for (int i = 0; i < yDigits.length; i++) {
      int r = result;
      for (int j = 0; j < xDigits.length; j++) {
        result += (xDigits[j] * yDigits[i]) << (i + j);
      }
      result=r;
      for (int j = 0; j < xDigits.length; j++) {
        result += (xDigits[j] * yDigits[i]) << (i + j);
      }
    }

    return result;
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Метод Карацубы'),
      ),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          children: [
            TextField(
              onChanged: (value) {
                number1 = value;
              },
              decoration: InputDecoration(labelText: 'Введите первое число'),
              keyboardType: TextInputType.number,
            ),
            TextField(
              onChanged: (value) {
                number2 = value;
              },
              decoration: InputDecoration(labelText: 'Введите второе число'),
              keyboardType: TextInputType.number,
            ),
            SizedBox(height: 20),
            ElevatedButton(
              onPressed: multiply,
              child: Text('Умножить'),
            ),
            SizedBox(height: 20),
            if (errorMessage.isNotEmpty)
              Text(errorMessage, style: TextStyle(color: Colors.red)),
            if (binary1.isNotEmpty)
              Text('Двоичное первое число: $binary1'),
            if (binary2.isNotEmpty)
              Text('Двоичное второе число: $binary2'),
            SizedBox(height: 20),
            if (result_kar.isNotEmpty)
              Text('Результат в 10-ой системе (методом Карацубы): $result_kar\n Время: $kar_t'),
            if (result_common.isNotEmpty)
              Text('Результат в 10-ой системе (обычное умножение): $result_common \n Время: $comm_t'),
          ],
        ),
      ),
    );
  }
}
