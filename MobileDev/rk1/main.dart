import 'package:flutter/material.dart';
import 'dart:math';

void main() {
  runApp(PolynomialGraphApp());
}

class PolynomialGraphApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      debugShowCheckedModeBanner: false,
      title: 'Polynomial Graph',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: PolynomialInput(),
    );
  }
}

class PolynomialInput extends StatefulWidget {
  @override
  _PolynomialInputState createState() => _PolynomialInputState();
}

class _PolynomialInputState extends State<PolynomialInput> {
  final TextEditingController _xController = TextEditingController();
  final List<TextEditingController> _coeffControllers =
      List.generate(4, (index) => TextEditingController());
  List<double> _coefficients = [0, 0, 0, 0];

  void _generateGraph() {
    setState(() {
      double x = double.tryParse(_xController.text) ?? 0;

      _coefficients = _coeffControllers.map((controller) {
        double value = double.tryParse(controller.text) ?? 0;
        return value;
      }).toList();

      print('x: $x');
      print('Коэффициенты: $_coefficients');
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Polynomial Graph'),
      ),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          children: <Widget>[
            SizedBox(height: 10),
            for (int i = 0; i < 4; i++)
              TextField(
                controller: _coeffControllers[i],
                decoration: InputDecoration(labelText: 'Коэффициент a${i}'),
                keyboardType: TextInputType.number,
              ),
            SizedBox(height: 10),
            ElevatedButton(
              onPressed: _generateGraph,
              child: Text('Сгенерировать график'),
            ),
            SizedBox(height: 20),
            Expanded(
              child: CustomPaint(
                painter: PolynomialGraphPainter(coefficients: _coefficients),
                child: Container(),
              ),
            ),
          ],
        ),
      ),
    );
  }
}

class PolynomialGraphPainter extends CustomPainter {
  final List<double> coefficients;

  PolynomialGraphPainter({required this.coefficients});

  void paint(Canvas canvas, Size size) {
    final paint = Paint()
      ..color = Colors.blue
      ..style = PaintingStyle.stroke
      ..strokeWidth = 4.0;

    final axisPaint = Paint()
      ..color = Colors.black
      ..style = PaintingStyle.stroke
      ..strokeWidth = 2.0;

    // Увеличение диапазона масштабов
    const double yMax = 10000; // Измените это значение для увеличения диапазона по оси Y
    const double xMax = 10; // Измените это значение для увеличения диапазона по оси X

    final textStyle = TextStyle(
      color: Colors.black,
      fontSize: 12,
    );
    final textPainter = TextPainter(
      textDirection: TextDirection.ltr,
    );

    // Рисуем оси
    canvas.drawLine(Offset(0, size.height), Offset(size.width, size.height), axisPaint); // Ось X
    canvas.drawLine(Offset(0, 0), Offset(0, size.height), axisPaint); // Ось Y

    // Рисуем график
    final path = Path();
    for (double x = 0; x <= xMax; x++) {
      double y = _calculatePolynomialValue(x);
      double normalizedY = size.height - (y / yMax) * size.height;

      if (x == 0) {
        path.moveTo((x / xMax) * size.width, normalizedY);
      } else {
        path.lineTo((x / xMax) * size.width, normalizedY);
      }
    }

    canvas.drawPath(path, paint);

    // Добавление чисел на ось X
    for (int i = 0; i <= 10; i++) { // Предположим, 10 делений по оси X
      double x = i * 10;
      double xPos = (x / 100) * size.width;

      textPainter.text = TextSpan(
        text: x.toString(),
        style: textStyle,
      );
      textPainter.layout();
      textPainter.paint(canvas, Offset(xPos - textPainter.width / 2, size.height));
    }
  }



  double _calculatePolynomialValue(double x) {
    double result = 0.0;
    for (int i = 0; i < coefficients.length; i++) {
      result +=
          coefficients[i] * pow(x, i); // y = a0 + a1 * x + a2 * x^2 + a3 * x^3
    }
    return result;
  }

  @override
  bool shouldRepaint(CustomPainter oldDelegate) {
    return true; // Перерисовываем график при изменении состояния
  }
}
