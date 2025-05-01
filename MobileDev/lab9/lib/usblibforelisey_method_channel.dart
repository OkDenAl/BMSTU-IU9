import 'package:flutter/foundation.dart';
import 'package:flutter/services.dart';

import 'usblibforelisey_platform_interface.dart';

/// An implementation of [UsblibforeliseyPlatform] that uses method channels.
class MethodChannelUsblibforelisey extends UsblibforeliseyPlatform {
  final _channel = const MethodChannel('usblibforelisey');

  static registerWith() {
    UsblibforeliseyPlatform.instance = MethodChannelUsblibforelisey();
  }

  @override
  Future<bool> init() async {
    return true;
  }

  @override
  Future<void> exit() async {
    return;
  }

  @override
  Future<bool> connect(int index) async {
    return await _channel.invokeMethod('connect', {"index": index});
  }

  @override
  Future<bool> hasPermission(int index) async {
    return await _channel.invokeMethod('hasPermission', {"index": index});
  }

  @override
  Future<bool> hasAccessoryConnected() async {
    return await _channel.invokeMethod('hasAccessoryConnected');
  }

  @override
  Future<Uint8List> read() async {
    List<dynamic> data = await _channel.invokeMethod('read');
    return Uint8List.fromList(data.cast<int>());
  }

  @override
  Future<bool> requestPermission(int index) async {
    return await _channel.invokeMethod('requestPermission', {"index": index});
  }

  @override
  Future<bool> write(Uint8List data) async {
    return await _channel.invokeMethod('write', {"data": data});
  }
}
