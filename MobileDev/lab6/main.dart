import 'package:flutter/material.dart';
import 'package:web_socket_channel/web_socket_channel.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Lab6',
      theme: ThemeData(
        primarySwatch: Colors.blue,
      ),
      home: NumberForm(),
      debugShowCheckedModeBanner: false,
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
  late WebSocketChannel channel;

  @override
  void initState() {
    super.initState();
    // Подключение к WebSocket-серверу
    channel = WebSocketChannel.connect(
      Uri.parse('ws://195.19.41.28:8090/'), // Обратите внимание, что это WebSocket URL
    );

    channel.stream.listen((data) {
      setState(() {
        _currentNumber = int.parse(data);
      });
    });
  }

  @override
  void dispose() {
    channel.sink.close(); // Закрытие канала при выходе из виджета
    super.dispose();
  }

  void sendNumber() {
    channel.sink.add(_currentNumber.toString());
  }

  void sendNumberFromNumberController() {
    channel.sink.add(_numberController.text);
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

  void getNumber() {
    channel.sink.add('GET_NUMBER');
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Lab6'),
      ),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            children: <Widget>[
              TextFormField(
                controller: _numberController,
                decoration: InputDecoration(labelText: 'Напишите число'),
                keyboardType: TextInputType.number,
                validator: (value) {
                  if (value == null || value.isEmpty) {
                    return 'Напишите число';
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
                child: Text('Отправить число'),
              ),
              SizedBox(height: 20),
              ElevatedButton(
                onPressed: getNumber,
                child: Text('Получить число'),
              ),
              const SizedBox(height: 20),
              Text('Число: $_currentNumber', style: TextStyle(fontSize: 20)),
              SizedBox(height: 20),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  ElevatedButton(
                    onPressed: decrement,
                    child: Text('- Уменьшить'),
                  ),
                  SizedBox(width: 20),
                  ElevatedButton(
                    onPressed: increment,
                    child: Text('+ Увеличить'),
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