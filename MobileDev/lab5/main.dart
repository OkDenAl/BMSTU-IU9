import 'dart:async';
import 'dart:io';
import 'package:flutter/material.dart';
import 'package:mqtt_client/mqtt_client.dart';
import 'package:mqtt_client/mqtt_server_client.dart';

class Lab5 extends StatefulWidget {
  @override
  State<Lab5> createState() => MqttFormState();
}

class MqttFormState extends State<Lab5> {
  final _formKey = GlobalKey<FormState>();
  String _message1 = "";
  String _message2 = "";
  String _message3 = "";
  String _receivedMessage = "";
  String _receivedMessageBuf = "";

  final client = MqttServerClient('test.mosquitto.org', '');

  var pongCount = 0; // Счетчик Pong

  @override
  void initState() {
    super.initState();
    connect();
  }

  Future<void> connect() async {
    client.logging(on: true);
    client.setProtocolV311();
    client.keepAlivePeriod = 20;
    client.onDisconnected = onDisconnected;
    client.onConnected = onConnected;
    client.onSubscribed = onSubscribed;
    client.pongCallback = pong;

    print('Mosquitto client connecting....');

    try {
      await client.connect();
    } on NoConnectionException catch (e) {
      print('client exception - $e');
      client.disconnect();
    } on SocketException catch (e) {
      print('socket exception - $e');
      client.disconnect();
    }

    if (client.connectionStatus!.state == MqttConnectionState.connected) {
      print('Mosquitto client connected');

      client.subscribe('UI9/a', MqttQos.exactlyOnce);
      client.subscribe('UI9/b', MqttQos.exactlyOnce);
      client.subscribe('UI9/c', MqttQos.exactlyOnce);

      client.updates!.listen((List<MqttReceivedMessage<MqttMessage?>>? c) {
        final recMess = c![0].payload as MqttPublishMessage;
        final pt =
            MqttPublishPayload.bytesToStringAsString(recMess.payload.message);
        setState(() {
          _receivedMessageBuf +=
              pt + ' '; // Обновить состояние с полученным сообщением
        });
      });
    } else {
      print(
          'ERROR Mosquitto client connection failed - disconnecting, status is ${client.connectionStatus}');
      client.disconnect();
    }
  }

  Future<void> sendMessage(String pubTopic, String msg) async {
    final builder = MqttClientPayloadBuilder();

    builder.addString('$msg');

    print('Publishing to topic $pubTopic');
    client.publishMessage(pubTopic, MqttQos.exactlyOnce, builder.payload!);
  }

  void onSubscribed(String topic) {
    print('Subscription confirmed for topic $topic');
  }

  void onDisconnected() {
    print('OnDisconnected client callback - Client disconnection');
    if (client.connectionStatus!.disconnectionOrigin ==
        MqttDisconnectionOrigin.solicited) {
      print('OnDisconnected callback is solicited, this is correct');
    } else {
      print(
          'OnDisconnected callback is unsolicited or none, this is incorrect - exiting');
      exit(-1);
    }
  }

  void onConnected() {
    print('OnConnected client callback - Client connection was successful');
  }

  void pong() {
    print('Ping response client callback invoked');
    pongCount++;
  }

  @override
  void dispose() {
    client.disconnect();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: <Widget>[
              TextFormField(
                decoration: InputDecoration(labelText: 'Введите сообщение'),
                onChanged: (value) {
                  _message1 = value;
                },
                validator: (value) {
                  if (value == null || value.isEmpty) {
                    return 'Пожалуйста, введите сообщение';
                  }
                  return null;
                },
              ),
              SizedBox(height: 16),
              TextFormField(
                decoration: InputDecoration(labelText: 'Введите сообщение'),
                onChanged: (value) {
                  _message2 = value;
                },
                validator: (value) {
                  if (value == null || value.isEmpty) {
                    return 'Пожалуйста, введите сообщение';
                  }
                  return null;
                },
              ),
              SizedBox(height: 16),
              TextFormField(
                decoration: InputDecoration(labelText: 'Введите сообщение'),
                onChanged: (value) {
                  _message3 = value;
                },
                validator: (value) {
                  if (value == null || value.isEmpty) {
                    return 'Пожалуйста, введите сообщение';
                  }
                  return null;
                },
              ),
              SizedBox(height: 16),
              ElevatedButton(
                onPressed: () {
                  if (_formKey.currentState!.validate()) {
                    sendMessage("UI9/a", _message1);
                    sendMessage("UI9/b", _message2);
                    sendMessage("UI9/c", _message3);
                  }
                },
                child: Text('Отправить сообщение'),
              ),
              SizedBox(height: 20),
              ElevatedButton(
                onPressed: () {
                  setState(() {
                    _receivedMessage = _receivedMessageBuf; // Обновляем текст
                    _receivedMessageBuf = '';
                  });
                },
                child: Text('Получить сообщения'),
              ),
              Text('Полученное сообщение: $_receivedMessage'),
            ],
          ),
        ),
      ),
    );
  }
}

void main() => runApp(new MaterialApp(
    debugShowCheckedModeBanner: false,
    home: new Scaffold(
        appBar: new AppBar(title: new Text('IU9 - Форма ввода')),
        body: new Lab5())));
