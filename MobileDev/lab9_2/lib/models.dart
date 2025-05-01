class UsbAccessory {
  final int index;
  final int model;
  final int manufacturer;
  final int serial;

  UsbAccessory({
    required this.index,
    required this.model,
    required this.manufacturer,
    required this.serial,
  });

  factory UsbAccessory.fromMap(Map<dynamic, dynamic> map) {
    return UsbAccessory(
      index: map['index'],
      model: map['model'],
      manufacturer: map['manufacturer'],
      serial: map['serial'],
    );
  }

  Map<String, dynamic> toMap() {
    return {
      'index': index,
      'model': model,
      'manufacturer': manufacturer,
      'serial': serial,
    };
  }

  @override
  String toString() => toMap().toString();
}