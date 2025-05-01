import 'dart:typed_data';

import 'package:plugin_platform_interface/plugin_platform_interface.dart';

import 'usblibforelisey_method_channel.dart';

abstract class UsblibforeliseyPlatform extends PlatformInterface {
  /// Constructs a UsblibforeliseyPlatform.
  UsblibforeliseyPlatform() : super(token: _token);

  static final Object _token = Object();

  static UsblibforeliseyPlatform _instance = MethodChannelUsblibforelisey();

  /// The default instance of [UsblibforeliseyPlatform] to use.
  ///
  /// Defaults to [MethodChannelUsblibforelisey].
  static UsblibforeliseyPlatform get instance => _instance;

  /// Platform-specific implementations should set this with their own
  /// platform-specific class that extends [UsblibforeliseyPlatform] when
  /// they register themselves.
  static set instance(UsblibforeliseyPlatform instance) {
    PlatformInterface.verifyToken(instance, _token);
    _instance = instance;
  }

  Future<bool> hasAccessoryConnected();

  Future<bool> hasPermission(int index);

  Future<bool> requestPermission(int index);

  Future<bool> connect(int index);

  Future<Uint8List> read();

  Future<bool> write(Uint8List data);
}
