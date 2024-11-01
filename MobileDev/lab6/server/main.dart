import 'dart:io';

Future<void> main() async {
  final server = await HttpServer.bind(InternetAddress.anyIPv4, 8090);
  print('Сервер запущен на http://${server.address.host}:${server.port}/');

  await for (var request in server) {
    if (WebSocketTransformer.isUpgradeRequest(request)) {
      final socket = await WebSocketTransformer.upgrade(request);
      handleWebSocket(socket);
    } else {
      request.response.statusCode = HttpStatus.methodNotAllowed;
      request.response.write('Метод не разрешен');
      request.response.close();
    }
  }
}

void handleWebSocket(WebSocket socket) async {
  print('Клиент подключен');
  socket.listen((message) async {
    try {
      if (message == 'GET_NUMBER') {
        await sendSavedNumber(socket);
      } else {
        int number = int.parse(message);

        final file = File('data.txt');
        await file.writeAsString('$number');

        print('Число сохранено: $number');
        socket.add('Число сохранено: $number');
      }
    } catch (e) {
      socket.add('Ошибка при обработке сообщения: ${e.toString()}');
    }
  }, onDone: () {
    print('Клиент отключен');
  }, onError: (error) {
    print('Ошибка: $error');
  });
}

Future<void> sendSavedNumber(WebSocket socket) async {
  final file = File('data.txt');
  if (await file.exists()) {
    String content = await file.readAsString();
    print('Отправляем сохраненное число: $content');
    socket.add(content);
  } else {
    socket.add('Файл не найден.');
  }
}