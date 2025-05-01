import 'package:flutter/material.dart';
import 'dart:async';

import 'package:flutter/services.dart';
import 'package:usblibforelisey/usblibforelisey.dart';

void main() {
  runApp(const MyApp());
}

class MyApp extends StatefulWidget {
  const MyApp({super.key});

  @override
  State<MyApp> createState() => _MyAppState();
}

class _MyAppState extends State<MyApp> {
  String _connectionStatus = 'Unknown';
  String _readData = 'No data received';
  TextEditingController _textEditingController = TextEditingController();
  final _usblibforeliseyPlugin = Usblibforelisey();

  @override
  void initState() {
    super.initState();
    initPlatformState();
  }

  // Platform messages are asynchronous, so we initialize in an async method.
  Future<void> initPlatformState() async {
    String connectionStatus;
    // Platform messages may fail, so we use a try/catch PlatformException.
    // We also handle the message potentially returning null.
    try {
      connectionStatus = await status();
    } catch (e) {
      connectionStatus = 'Some error!';
    }

    // If the widget was removed from the tree while the asynchronous platform
    // message was in flight, we want to discard the reply rather than calling
    // setState to update our non-existent appearance.
    if (!mounted) return;

    setState(() {
      _connectionStatus = connectionStatus;
    });

    if (connectionStatus == 'Connected!'){
      await read();
      return;
    }

    await Future.delayed(Duration(seconds: 1));
    await initPlatformState();
  }

  Future<String> status() async {
    var hasAccessory = await _usblibforeliseyPlugin.hasAccessoryConnected();
    if (!hasAccessory) {
      return 'No devices';
    }

    var hasPermission = await _usblibforeliseyPlugin.hasPermission(0);
    if (!hasPermission) {
      await _usblibforeliseyPlugin.requestPermission(0);
      return 'No permission!';
    }

    await _usblibforeliseyPlugin.connect(0);
    return 'Connected!';
  }

  Future<void> read() async {
    var data = await _usblibforeliseyPlugin.read();
    setState(() {
      _readData = String.fromCharCodes(data);
    });

    await Future.delayed(const Duration(milliseconds: 500));
    await read();
  }

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      home: Scaffold(
        appBar: AppBar(
          title: const Text('Plugin example app'),
        ),
        body: Center(
          child: Column(
            children: [
              Text('Connection status: $_connectionStatus\n'),
              TextField(
                controller: _textEditingController,
                decoration: InputDecoration(
                  hintText: 'Enter your message',
                  border: OutlineInputBorder(),
                ),
              ),
              SizedBox(height: 10),
              ElevatedButton(
                onPressed: () async {
                  if (_textEditingController.text.isEmpty) {
                    return;
                  }

                  await _usblibforeliseyPlugin.write(Uint8List.fromList(
                      _textEditingController.text.codeUnits));
                  return;
                },
                child: Text('Send'),
              ),
              SizedBox(height: 20),

              // Large label at the bottom
              Text(
                'Reader: $_readData',
                style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
