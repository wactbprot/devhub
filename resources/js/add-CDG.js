/**
 * Wandelt bestimmte 4 Bytes, des Anybus Modbus-Profibus-Gateways
 * Standardtelegram 202 ("ExS,PVF,ChS,TpS")  (siehe Datei "CDGX0B7E.GSD")
 * muss konfiguriert sein.
 *
 * Die vom Gateway empfangenen Bytes müssen nicht umsortiert werden.
 * Das Anybus-Gateway liefert 10Byte lange Resultate.
 *
 * @param b Buffer buf Rückgabe des Modbus-Profibus-Gateways
 * @param a Number Profibus-Adresse (default: 1)
 * @param n Number Anzahl der Druckwerte (default: 1)
 * @return Object (o) mit Property o.Status Int und o.Value Number/Array Float-Zahl(en)
 */
 function readAnybusGw(b, a, n) {
   var ret = []
     , ba  = []
     , pos
     , err
     , adr = parseInt(a)
     , cnt = parseInt(n)
     , l = 8;

   if (isNaN(adr)) adr = 1;
   if (isNaN(cnt)) cnt = 1;
   for (var i = 0; i < cnt; i++) {
     pos = l * (adr + i) - l;
     ba = [ b[pos + 1]
          , b[pos + 2]
          , b[pos + 3]
          , b[pos + 4]];
     var nb = new Buffer(ba);
     ret.push(nb.readFloatBE(0));
   }
   return (ret.length > 1) ? ret : ret[0];
 }
 exports.readAnybusGw = readAnybusGw;


/**
 * Funktioniert(e) für Danfos MKS CDG (2018). 
 * Datagram: PO,PI,ExS,PVF
 * Profibusmaster: Kunbus
 *
 * @param b Buffer buf Rückgabe des Modbus-Profibus-Gateways
 * @return Object (o) mit Property o.Status Int und o.Value Number/Array Float-Zahl(en)
 */
 function readKunbusGw(b) {
   var ba  = [], pos = 6;
     
     ba = [ b[pos + 0]
          , b[pos + 3]
          , b[pos + 2]
	    ,0];
     var nb = new Buffer(ba);
    return nb.readFloatBE(0)
 }
 exports.readKunbusGw = readKunbusGw;

