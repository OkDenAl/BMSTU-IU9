import 'dart:convert';
import 'dart:io';

Future<void> main() async {
  final server = await HttpServer.bind(InternetAddress.anyIPv4, 8090);
  print('Сервер запущен на http://${server.address.host}:${server.port}/');

  await for (var request in server) {
    if (request.method == 'POST') {
      await handlePost(request);
    } else if (request.method == 'GET') {
      await handleGet(request);
    } else {
      request.response.statusCode = HttpStatus.methodNotAllowed;
      request.response.write('Метод не разрешен');
      request.response.close();
    }
  }
}

Future<void> handlePost(HttpRequest request) async {
  try {
    String content = await utf8.decoder.bind(request).join();
    int number = int.parse(content);

    // Сохраняем число в файл
    final file = File('data.txt');
    await file.writeAsString('$number');

    print('Число сохранено: $number');

    request.response
      ..statusCode = HttpStatus.ok
      ..write(number)
      ..close();
  } catch (e) {
    request.response
      ..statusCode = HttpStatus.badRequest
      ..write('Ошибка при обработке запроса: ${e.toString()}')
      ..close();
  }
}

Future<void> handleGet(HttpRequest request) async {
  final file = File('data.txt');
  if (await file.exists()) {
    String content = await file.readAsString();
    print('Сохраненное число: $content');
    request.response
      ..statusCode = HttpStatus.ok
      ..write(content)
      ..close();
  } else {
    request.response
      ..statusCode = HttpStatus.notFound
      ..write('Файл не найден.')
      ..close();
  }
}