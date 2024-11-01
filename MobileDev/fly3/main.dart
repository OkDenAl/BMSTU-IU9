import 'package:flutter/material.dart';
import 'package:mysql1/mysql1.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'MySQL App',
      home: UserForm(),
    );
  }
}

class UserForm extends StatefulWidget {
  @override
  _UserFormState createState() => _UserFormState();
}

class _UserFormState extends State<UserForm> {
  final _formKey = GlobalKey<FormState>();
  String _name = '';
  String _email = '';
  int _age = 0;
  List<Map> _users = [];

  Future<MySqlConnection> _getConnection() async {
    return await MySqlConnection.connect(
      ConnectionSettings(
        host: 'students.yss.su',
        port: 3306,
        user: 'iu9mobile',
        db: 'iu9mobile',
        password: 'bmstubmstu123',
      ),
    );
  }

  Future<void> _createTable() async {
    final conn = await _getConnection();
    await conn.query('''
      CREATE TABLE IF NOT EXISTS Okutin (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(100),
        email VARCHAR(100),
        age INT
      )
    ''');
    await conn.close();
  }

  Future<void> _saveUser() async {
    final conn = await _getConnection();
    await conn.query('INSERT INTO users (name, email, age) VALUES (?, ?, ?)',
        [_name, _email, _age]);
    await conn.close();
  }

  Future<void> _fetchUsers() async {
    final conn = await _getConnection();
    var results = await conn.query('SELECT name, email, age FROM users');
    setState(() {
      _users = results
          .map((row) => {'name': row[0], 'email': row[1], 'age': row[2]})
          .toList();
    });
    await conn.close();
  }

  Future<void> _deleteUsers() async {
    final conn = await _getConnection();
    await conn.query('DELETE FROM users');
    await conn.close();
  }

  @override
  void initState() {
    super.initState();
    _createTable(); // Создание таблицы при старте приложения
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('User Management')),
      body: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Form(
          key: _formKey,
          child: Column(
            children: [
              TextFormField(
                decoration: InputDecoration(labelText: 'Имя'),
                onSaved: (value) => _name = value!,
              ),
              TextFormField(
                decoration: InputDecoration(labelText: 'Емейл'),
                onSaved: (value) => _email = value!,
              ),
              TextFormField(
                decoration: InputDecoration(labelText: 'Возраст'),
                keyboardType: TextInputType.number,
                onSaved: (value) => _age = int.tryParse(value!) ?? 0,
              ),
              SizedBox(height: 20),
              ElevatedButton(
                onPressed: () {
                  if (_formKey.currentState!.validate()) {
                    _formKey.currentState!.save();
                    _saveUser();
                  }
                },
                child: Text('Сохранить пользователя'),
              ),
              ElevatedButton(
                onPressed: () {
                  _fetchUsers();
                },
                child: Text('Получить всех пользователей'),
              ),
              ElevatedButton(
                onPressed: () {
                  _deleteUsers();
                },
                child: Text('Удалить всех пользователей'),
              ),
              Expanded(
                child: ListView.builder(
                  itemCount: _users.length,
                  itemBuilder: (context, index) {
                    return ListTile(
                      title: Text(
                          '${_users[index]['name']} (${_users[index]['age']})'),
                      subtitle: Text(_users[index]['email']),
                    );
                  },
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }
}
