import 'package:flutter/material.dart';
import 'package:http/http.dart' as http;

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Lab3',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: NumberForm(),
    );
  }
}

class NumberForm extends StatefulWidget {
  @override
  _NumberFormState createState() => _NumberFormState();
}

class _NumberFormState extends State<NumberForm> {
  final _formKey = GlobalKey<FormState>();
  final _numberController = TextEditingController();
  int _currentNumber = 0;

  Future<void> sendNumber() async {
    final url = Uri.parse('http://195.19.55.124:8080/');
    final response = await http.post(
      url,
      headers: {
        'Content-Type': 'text/plain',
      },
      body: '$_currentNumber',
    );

    if (response.statusCode == 200) {
      print('Num sent: ${_currentNumber}');
    } else {
      print('Error: ${response.statusCode}');
    }
  }
  Future<void> sendNumberFromNumberController() async {
    final url = Uri.parse('http://195.19.55.124:8080/');
    final response = await http.post(
      url,
      headers: {
        'Content-Type': 'text/plain',
      },
      body: _numberController.text,
    );

    if (response.statusCode == 200) {
      print('Num sent: ${_numberController.text}');
    } else {
      print('Error: ${response.statusCode}');
    }
  }

  Future<void> getNumber() async {
    final url = Uri.parse('http://195.19.55.124:8080/');

    final response = await http.get(url);

    if (response.statusCode == 200) {
      setState(() {
        _currentNumber = int.parse(response.body);
      });

      print('Num: $_currentNumber');
    } else {
      print('Error: ${response.statusCode}');
    }
  }

  void increment() {
    setState(() {
      _currentNumber++;
    });

    sendNumber();
  }

  void decrement() {
    setState(() {
      _currentNumber--;
    });

    sendNumber();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Lab2'),
      ),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            children: <Widget>[
              TextFormField(
                controller: _numberController,
                decoration: InputDecoration(labelText: 'Write num'),
                keyboardType: TextInputType.number,
                validator: (value) {
                  if (value == null || value.isEmpty) {
                    return 'Write num';
                  }
                  return null;
                },
              ),
              const SizedBox(height: 20),
              ElevatedButton(
                onPressed: () {
                  if (_formKey.currentState!.validate()) {
                    sendNumberFromNumberController();
                  }
                },
                child: Text('Text'),
              ),
              SizedBox(height: 20),
              ElevatedButton(
                onPressed: getNumber,
                child: Text('Get num'),
              ),
              const SizedBox(height: 20),
              Text('Current: $_currentNumber', style: TextStyle(fontSize: 20)),
              SizedBox(height: 20),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  ElevatedButton(
                    onPressed: decrement,
                    child: Text('- Less'),
                  ),
                  SizedBox(width: 20),
                  ElevatedButton(
                    onPressed: increment,
                    child: Text('+ Bigger'),
                  ),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }
}