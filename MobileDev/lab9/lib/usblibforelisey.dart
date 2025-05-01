import 'dart:typed_data';

import 'usblibforelisey_platform_interface.dart';

UsblibforeliseyPlatform get _platform => UsblibforeliseyPlatform.instance;

class Usblibforelisey {
  Future<bool> hasAccessoryConnected() => _platform.hasAccessoryConnected();

  Future<bool> hasPermission(int index) => _platform.hasPermission(index);

  Future<bool> requestPermission(int index) => _platform.requestPermission(index);

  Future<bool> connect(int index) => _platform.connect(index);

  Future<Uint8List> read() => _platform.read();

  Future<bool> write(Uint8List data) => _platform.write(data);
}
