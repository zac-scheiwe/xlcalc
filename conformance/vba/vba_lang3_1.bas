Attribute VB_Name = "VbaLang3_1"
Option Explicit

Private Function fV(x As Variant) As Variant
    fV = x
End Function
Private Function Z() As Long
    Z = 0
End Function
Private Sub ByRefL(ByRef n As Long)
    n = n + 1
End Sub
Private Sub ByValL(ByVal n As Long)
    n = n + 1
End Sub
Private Sub ByRefV(ByRef v As Variant)
    v = v & "x"
End Sub
Private Sub ByRefI(ByRef n As Integer)
    n = n + 1
End Sub
Private Function Opt1(Optional a As Variant, Optional b As Long = 7, Optional c As String = "d") As String
    Opt1 = IIf(IsMissing(a), "M", "a=" & a) & "|" & b & "|" & c
End Function
Private Function Opt2(Optional a As Long, Optional b As Variant = Empty) As String
    Opt2 = a & "|" & TypeName(b)
End Function
Private Function PA(ParamArray p() As Variant) As String
    Dim i As Long, s As String
    s = LBound(p) & ":" & UBound(p) & ":"
    For i = LBound(p) To UBound(p)
        s = s & TypeName(p(i)) & ","
    Next i
    PA = s
End Function
Private Function PA2(ByVal a As Long, ParamArray p() As Variant) As String
    PA2 = a & ":" & (UBound(p) - LBound(p) + 1)
End Function
Private Function StatCnt() As Long
    Static n As Long
    n = n + 1
    StatCnt = n
End Function
Private Static Function StatAll() As String
    Dim q As Long
    q = q + 1
    StatAll = CStr(q)
End Function
Private Property Get PG() As Long
    PG = 42
End Property
Private Function Gs(ByVal n As Long) As String
    Dim s As String
    s = "a"
    GoSub Sub1
    s = s & "b"
    If n = 1 Then GoSub Sub2
    Gs = s
    Exit Function
Sub1:
    s = s & "1"
    Return
Sub2:
    s = s & "2"
    Return
End Function
Private Function Dump(v As Variant) As String
    Dim s As String, i As Long
    If IsArray(v) Then
        For i = LBound(v) To UBound(v)
            s = s & Dump(v(i)) & ","
        Next i
        Dump = "[" & s & "]"
    ElseIf IsNull(v) Then
        Dump = "Null"
    ElseIf IsEmpty(v) Then
        Dump = "Empty"
    ElseIf IsObject(v) Then
        Dump = "<" & TypeName(v) & ">"
    Else
        Dump = TypeName(v) & ":" & CStr(v)
    End If
End Function

Public Function LangThree1(ByVal k As Long) As Variant
    Select Case (k - 1000) \ 100
    Case 0: LangThree1 = MC0(k)
    Case 1: LangThree1 = MC1(k)
    Case 2: LangThree1 = MC2(k)
    Case 3: LangThree1 = MC3(k)
    Case 4: LangThree1 = MC4(k)
    Case 5: LangThree1 = MC5(k)
    Case 6: LangThree1 = MC6(k)
    Case 7: LangThree1 = MC7(k)
    Case 8: LangThree1 = MC8(k)
    Case 9: LangThree1 = MC9(k)
    End Select
End Function

Private Function MC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1000: MC0 = M1000()
    Case 1001: MC0 = M1001()
    Case 1002: MC0 = M1002()
    Case 1003: MC0 = M1003()
    Case 1004: MC0 = M1004()
    Case 1005: MC0 = M1005()
    Case 1006: MC0 = M1006()
    Case 1007: MC0 = M1007()
    Case 1008: MC0 = M1008()
    Case 1009: MC0 = M1009()
    Case 1010: MC0 = M1010()
    Case 1011: MC0 = M1011()
    Case 1012: MC0 = M1012()
    Case 1013: MC0 = M1013()
    Case 1014: MC0 = M1014()
    Case 1015: MC0 = M1015()
    Case 1016: MC0 = M1016()
    Case 1017: MC0 = M1017()
    Case 1018: MC0 = M1018()
    Case 1019: MC0 = M1019()
    Case 1020: MC0 = M1020()
    Case 1021: MC0 = M1021()
    Case 1022: MC0 = M1022()
    Case 1023: MC0 = M1023()
    Case 1024: MC0 = M1024()
    Case 1025: MC0 = M1025()
    Case 1026: MC0 = M1026()
    Case 1027: MC0 = M1027()
    Case 1028: MC0 = M1028()
    Case 1029: MC0 = M1029()
    Case 1030: MC0 = M1030()
    Case 1031: MC0 = M1031()
    Case 1032: MC0 = M1032()
    Case 1033: MC0 = M1033()
    Case 1034: MC0 = M1034()
    Case 1035: MC0 = M1035()
    Case 1036: MC0 = M1036()
    Case 1037: MC0 = M1037()
    Case 1038: MC0 = M1038()
    Case 1039: MC0 = M1039()
    Case 1040: MC0 = M1040()
    Case 1041: MC0 = M1041()
    Case 1042: MC0 = M1042()
    Case 1043: MC0 = M1043()
    Case 1044: MC0 = M1044()
    Case 1045: MC0 = M1045()
    Case 1046: MC0 = M1046()
    Case 1047: MC0 = M1047()
    Case 1048: MC0 = M1048()
    Case 1049: MC0 = M1049()
    Case 1050: MC0 = M1050()
    Case 1051: MC0 = M1051()
    Case 1052: MC0 = M1052()
    Case 1053: MC0 = M1053()
    Case 1054: MC0 = M1054()
    Case 1055: MC0 = M1055()
    Case 1056: MC0 = M1056()
    Case 1057: MC0 = M1057()
    Case 1058: MC0 = M1058()
    Case 1059: MC0 = M1059()
    Case 1060: MC0 = M1060()
    Case 1061: MC0 = M1061()
    Case 1062: MC0 = M1062()
    Case 1063: MC0 = M1063()
    Case 1064: MC0 = M1064()
    Case 1065: MC0 = M1065()
    Case 1066: MC0 = M1066()
    Case 1067: MC0 = M1067()
    Case 1068: MC0 = M1068()
    Case 1069: MC0 = M1069()
    Case 1070: MC0 = M1070()
    Case 1071: MC0 = M1071()
    Case 1072: MC0 = M1072()
    Case 1073: MC0 = M1073()
    Case 1074: MC0 = M1074()
    Case 1075: MC0 = M1075()
    Case 1076: MC0 = M1076()
    Case 1077: MC0 = M1077()
    Case 1078: MC0 = M1078()
    Case 1079: MC0 = M1079()
    Case 1080: MC0 = M1080()
    Case 1081: MC0 = M1081()
    Case 1082: MC0 = M1082()
    Case 1083: MC0 = M1083()
    Case 1084: MC0 = M1084()
    Case 1085: MC0 = M1085()
    Case 1086: MC0 = M1086()
    Case 1087: MC0 = M1087()
    Case 1088: MC0 = M1088()
    Case 1089: MC0 = M1089()
    Case 1090: MC0 = M1090()
    Case 1091: MC0 = M1091()
    Case 1092: MC0 = M1092()
    Case 1093: MC0 = M1093()
    Case 1094: MC0 = M1094()
    Case 1095: MC0 = M1095()
    Case 1096: MC0 = M1096()
    Case 1097: MC0 = M1097()
    Case 1098: MC0 = M1098()
    Case 1099: MC0 = M1099()
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1100: MC1 = M1100()
    Case 1101: MC1 = M1101()
    Case 1102: MC1 = M1102()
    Case 1103: MC1 = M1103()
    Case 1104: MC1 = M1104()
    Case 1105: MC1 = M1105()
    Case 1106: MC1 = M1106()
    Case 1107: MC1 = M1107()
    Case 1108: MC1 = M1108()
    Case 1109: MC1 = M1109()
    Case 1110: MC1 = M1110()
    Case 1111: MC1 = M1111()
    Case 1112: MC1 = M1112()
    Case 1113: MC1 = M1113()
    Case 1114: MC1 = M1114()
    Case 1115: MC1 = M1115()
    Case 1116: MC1 = M1116()
    Case 1117: MC1 = M1117()
    Case 1118: MC1 = M1118()
    Case 1119: MC1 = M1119()
    Case 1120: MC1 = M1120()
    Case 1121: MC1 = M1121()
    Case 1122: MC1 = M1122()
    Case 1123: MC1 = M1123()
    Case 1124: MC1 = M1124()
    Case 1125: MC1 = M1125()
    Case 1126: MC1 = M1126()
    Case 1127: MC1 = M1127()
    Case 1128: MC1 = M1128()
    Case 1129: MC1 = M1129()
    Case 1130: MC1 = M1130()
    Case 1131: MC1 = M1131()
    Case 1132: MC1 = M1132()
    Case 1133: MC1 = M1133()
    Case 1134: MC1 = M1134()
    Case 1135: MC1 = M1135()
    Case 1136: MC1 = M1136()
    Case 1137: MC1 = M1137()
    Case 1138: MC1 = M1138()
    Case 1139: MC1 = M1139()
    Case 1140: MC1 = M1140()
    Case 1141: MC1 = M1141()
    Case 1142: MC1 = M1142()
    Case 1143: MC1 = M1143()
    Case 1144: MC1 = M1144()
    Case 1145: MC1 = M1145()
    Case 1146: MC1 = M1146()
    Case 1147: MC1 = M1147()
    Case 1148: MC1 = M1148()
    Case 1149: MC1 = M1149()
    Case 1150: MC1 = M1150()
    Case 1151: MC1 = M1151()
    Case 1152: MC1 = M1152()
    Case 1153: MC1 = M1153()
    Case 1154: MC1 = M1154()
    Case 1155: MC1 = M1155()
    Case 1156: MC1 = M1156()
    Case 1157: MC1 = M1157()
    Case 1158: MC1 = M1158()
    Case 1159: MC1 = M1159()
    Case 1160: MC1 = M1160()
    Case 1161: MC1 = M1161()
    Case 1162: MC1 = M1162()
    Case 1163: MC1 = M1163()
    Case 1164: MC1 = M1164()
    Case 1165: MC1 = M1165()
    Case 1166: MC1 = M1166()
    Case 1167: MC1 = M1167()
    Case 1168: MC1 = M1168()
    Case 1169: MC1 = M1169()
    Case 1170: MC1 = M1170()
    Case 1171: MC1 = M1171()
    Case 1172: MC1 = M1172()
    Case 1173: MC1 = M1173()
    Case 1174: MC1 = M1174()
    Case 1175: MC1 = M1175()
    Case 1176: MC1 = M1176()
    Case 1177: MC1 = M1177()
    Case 1178: MC1 = M1178()
    Case 1179: MC1 = M1179()
    Case 1180: MC1 = M1180()
    Case 1181: MC1 = M1181()
    Case 1182: MC1 = M1182()
    Case 1183: MC1 = M1183()
    Case 1184: MC1 = M1184()
    Case 1185: MC1 = M1185()
    Case 1186: MC1 = M1186()
    Case 1187: MC1 = M1187()
    Case 1188: MC1 = M1188()
    Case 1189: MC1 = M1189()
    Case 1190: MC1 = M1190()
    Case 1191: MC1 = M1191()
    Case 1192: MC1 = M1192()
    Case 1193: MC1 = M1193()
    Case 1194: MC1 = M1194()
    Case 1195: MC1 = M1195()
    Case 1196: MC1 = M1196()
    Case 1197: MC1 = M1197()
    Case 1198: MC1 = M1198()
    Case 1199: MC1 = M1199()
    End Select
    Exit Function
EH:
    MC1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1200: MC2 = M1200()
    Case 1201: MC2 = M1201()
    Case 1202: MC2 = M1202()
    Case 1203: MC2 = M1203()
    Case 1204: MC2 = M1204()
    Case 1205: MC2 = M1205()
    Case 1206: MC2 = M1206()
    Case 1207: MC2 = M1207()
    Case 1208: MC2 = M1208()
    Case 1209: MC2 = M1209()
    Case 1210: MC2 = M1210()
    Case 1211: MC2 = M1211()
    Case 1212: MC2 = M1212()
    Case 1213: MC2 = M1213()
    Case 1214: MC2 = M1214()
    Case 1215: MC2 = M1215()
    Case 1216: MC2 = M1216()
    Case 1217: MC2 = M1217()
    Case 1218: MC2 = M1218()
    Case 1219: MC2 = M1219()
    Case 1220: MC2 = M1220()
    Case 1221: MC2 = M1221()
    Case 1222: MC2 = M1222()
    Case 1223: MC2 = M1223()
    Case 1224: MC2 = M1224()
    Case 1225: MC2 = M1225()
    Case 1226: MC2 = M1226()
    Case 1227: MC2 = M1227()
    Case 1228: MC2 = M1228()
    Case 1229: MC2 = M1229()
    Case 1230: MC2 = M1230()
    Case 1231: MC2 = M1231()
    Case 1232: MC2 = M1232()
    Case 1233: MC2 = M1233()
    Case 1234: MC2 = M1234()
    Case 1235: MC2 = M1235()
    Case 1236: MC2 = M1236()
    Case 1237: MC2 = M1237()
    Case 1238: MC2 = M1238()
    Case 1239: MC2 = M1239()
    Case 1240: MC2 = M1240()
    Case 1241: MC2 = M1241()
    Case 1242: MC2 = M1242()
    Case 1243: MC2 = M1243()
    Case 1244: MC2 = M1244()
    Case 1245: MC2 = M1245()
    Case 1246: MC2 = M1246()
    Case 1247: MC2 = M1247()
    Case 1248: MC2 = M1248()
    Case 1249: MC2 = M1249()
    Case 1250: MC2 = M1250()
    Case 1251: MC2 = M1251()
    Case 1252: MC2 = M1252()
    Case 1253: MC2 = M1253()
    Case 1254: MC2 = M1254()
    Case 1255: MC2 = M1255()
    Case 1256: MC2 = M1256()
    Case 1257: MC2 = M1257()
    Case 1258: MC2 = M1258()
    Case 1259: MC2 = M1259()
    Case 1260: MC2 = M1260()
    Case 1261: MC2 = M1261()
    Case 1262: MC2 = M1262()
    Case 1263: MC2 = M1263()
    Case 1264: MC2 = M1264()
    Case 1265: MC2 = M1265()
    Case 1266: MC2 = M1266()
    Case 1267: MC2 = M1267()
    Case 1268: MC2 = M1268()
    Case 1269: MC2 = M1269()
    Case 1270: MC2 = M1270()
    Case 1271: MC2 = M1271()
    Case 1272: MC2 = M1272()
    Case 1273: MC2 = M1273()
    Case 1274: MC2 = M1274()
    Case 1275: MC2 = M1275()
    Case 1276: MC2 = M1276()
    Case 1277: MC2 = M1277()
    Case 1278: MC2 = M1278()
    Case 1279: MC2 = M1279()
    Case 1280: MC2 = M1280()
    Case 1281: MC2 = M1281()
    Case 1282: MC2 = M1282()
    Case 1283: MC2 = M1283()
    Case 1284: MC2 = M1284()
    Case 1285: MC2 = M1285()
    Case 1286: MC2 = M1286()
    Case 1287: MC2 = M1287()
    Case 1288: MC2 = M1288()
    Case 1289: MC2 = M1289()
    Case 1290: MC2 = M1290()
    Case 1291: MC2 = M1291()
    Case 1292: MC2 = M1292()
    Case 1293: MC2 = M1293()
    Case 1294: MC2 = M1294()
    Case 1295: MC2 = M1295()
    Case 1296: MC2 = M1296()
    Case 1297: MC2 = M1297()
    Case 1298: MC2 = M1298()
    Case 1299: MC2 = M1299()
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC3(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1300: MC3 = M1300()
    Case 1301: MC3 = M1301()
    Case 1302: MC3 = M1302()
    Case 1303: MC3 = M1303()
    Case 1304: MC3 = M1304()
    Case 1305: MC3 = M1305()
    Case 1306: MC3 = M1306()
    Case 1307: MC3 = M1307()
    Case 1308: MC3 = M1308()
    Case 1309: MC3 = M1309()
    Case 1310: MC3 = M1310()
    Case 1311: MC3 = M1311()
    Case 1312: MC3 = M1312()
    Case 1313: MC3 = M1313()
    Case 1314: MC3 = M1314()
    Case 1315: MC3 = M1315()
    Case 1316: MC3 = M1316()
    Case 1317: MC3 = M1317()
    Case 1318: MC3 = M1318()
    Case 1319: MC3 = M1319()
    Case 1320: MC3 = M1320()
    Case 1321: MC3 = M1321()
    Case 1322: MC3 = M1322()
    Case 1323: MC3 = M1323()
    Case 1324: MC3 = M1324()
    Case 1325: MC3 = M1325()
    Case 1326: MC3 = M1326()
    Case 1327: MC3 = M1327()
    Case 1328: MC3 = M1328()
    Case 1329: MC3 = M1329()
    Case 1330: MC3 = M1330()
    Case 1331: MC3 = M1331()
    Case 1332: MC3 = M1332()
    Case 1333: MC3 = M1333()
    Case 1334: MC3 = M1334()
    Case 1335: MC3 = M1335()
    Case 1336: MC3 = M1336()
    Case 1337: MC3 = M1337()
    Case 1338: MC3 = M1338()
    Case 1339: MC3 = M1339()
    Case 1340: MC3 = M1340()
    Case 1341: MC3 = M1341()
    Case 1342: MC3 = M1342()
    Case 1343: MC3 = M1343()
    Case 1344: MC3 = M1344()
    Case 1345: MC3 = M1345()
    Case 1346: MC3 = M1346()
    Case 1347: MC3 = M1347()
    Case 1348: MC3 = M1348()
    Case 1349: MC3 = M1349()
    Case 1350: MC3 = M1350()
    Case 1351: MC3 = M1351()
    Case 1352: MC3 = M1352()
    Case 1353: MC3 = M1353()
    Case 1354: MC3 = M1354()
    Case 1355: MC3 = M1355()
    Case 1356: MC3 = M1356()
    Case 1357: MC3 = M1357()
    Case 1358: MC3 = M1358()
    Case 1359: MC3 = M1359()
    Case 1360: MC3 = M1360()
    Case 1361: MC3 = M1361()
    Case 1362: MC3 = M1362()
    Case 1363: MC3 = M1363()
    Case 1364: MC3 = M1364()
    Case 1365: MC3 = M1365()
    Case 1366: MC3 = M1366()
    Case 1367: MC3 = M1367()
    Case 1368: MC3 = M1368()
    Case 1369: MC3 = M1369()
    Case 1370: MC3 = M1370()
    Case 1371: MC3 = M1371()
    Case 1372: MC3 = M1372()
    Case 1373: MC3 = M1373()
    Case 1374: MC3 = M1374()
    Case 1375: MC3 = M1375()
    Case 1376: MC3 = M1376()
    Case 1377: MC3 = M1377()
    Case 1378: MC3 = M1378()
    Case 1379: MC3 = M1379()
    Case 1380: MC3 = M1380()
    Case 1381: MC3 = M1381()
    Case 1382: MC3 = M1382()
    Case 1383: MC3 = M1383()
    Case 1384: MC3 = M1384()
    Case 1385: MC3 = M1385()
    Case 1386: MC3 = M1386()
    Case 1387: MC3 = M1387()
    Case 1388: MC3 = M1388()
    Case 1389: MC3 = M1389()
    Case 1390: MC3 = M1390()
    Case 1391: MC3 = M1391()
    Case 1392: MC3 = M1392()
    Case 1393: MC3 = M1393()
    Case 1394: MC3 = M1394()
    Case 1395: MC3 = M1395()
    Case 1396: MC3 = M1396()
    Case 1397: MC3 = M1397()
    Case 1398: MC3 = M1398()
    Case 1399: MC3 = M1399()
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1400: MC4 = M1400()
    Case 1401: MC4 = M1401()
    Case 1402: MC4 = M1402()
    Case 1403: MC4 = M1403()
    Case 1404: MC4 = M1404()
    Case 1405: MC4 = M1405()
    Case 1406: MC4 = M1406()
    Case 1407: MC4 = M1407()
    Case 1408: MC4 = M1408()
    Case 1409: MC4 = M1409()
    Case 1410: MC4 = M1410()
    Case 1411: MC4 = M1411()
    Case 1412: MC4 = M1412()
    Case 1413: MC4 = M1413()
    Case 1414: MC4 = M1414()
    Case 1415: MC4 = M1415()
    Case 1416: MC4 = M1416()
    Case 1417: MC4 = M1417()
    Case 1418: MC4 = M1418()
    Case 1419: MC4 = M1419()
    Case 1420: MC4 = M1420()
    Case 1421: MC4 = M1421()
    Case 1422: MC4 = M1422()
    Case 1423: MC4 = M1423()
    Case 1424: MC4 = M1424()
    Case 1425: MC4 = M1425()
    Case 1426: MC4 = M1426()
    Case 1427: MC4 = M1427()
    Case 1428: MC4 = M1428()
    Case 1429: MC4 = M1429()
    Case 1430: MC4 = M1430()
    Case 1431: MC4 = M1431()
    Case 1432: MC4 = M1432()
    Case 1433: MC4 = M1433()
    Case 1434: MC4 = M1434()
    Case 1435: MC4 = M1435()
    Case 1436: MC4 = M1436()
    Case 1437: MC4 = M1437()
    Case 1438: MC4 = M1438()
    Case 1439: MC4 = M1439()
    Case 1440: MC4 = M1440()
    Case 1441: MC4 = M1441()
    Case 1442: MC4 = M1442()
    Case 1443: MC4 = M1443()
    Case 1444: MC4 = M1444()
    Case 1445: MC4 = M1445()
    Case 1446: MC4 = M1446()
    Case 1447: MC4 = M1447()
    Case 1448: MC4 = M1448()
    Case 1449: MC4 = M1449()
    Case 1450: MC4 = M1450()
    Case 1451: MC4 = M1451()
    Case 1452: MC4 = M1452()
    Case 1453: MC4 = M1453()
    Case 1454: MC4 = M1454()
    Case 1455: MC4 = M1455()
    Case 1456: MC4 = M1456()
    Case 1457: MC4 = M1457()
    Case 1458: MC4 = M1458()
    Case 1459: MC4 = M1459()
    Case 1460: MC4 = M1460()
    Case 1461: MC4 = M1461()
    Case 1462: MC4 = M1462()
    Case 1463: MC4 = M1463()
    Case 1464: MC4 = M1464()
    Case 1465: MC4 = M1465()
    Case 1466: MC4 = M1466()
    Case 1467: MC4 = M1467()
    Case 1468: MC4 = M1468()
    Case 1469: MC4 = M1469()
    Case 1470: MC4 = M1470()
    Case 1471: MC4 = M1471()
    Case 1472: MC4 = M1472()
    Case 1473: MC4 = M1473()
    Case 1474: MC4 = M1474()
    Case 1475: MC4 = M1475()
    Case 1476: MC4 = M1476()
    Case 1477: MC4 = M1477()
    Case 1478: MC4 = M1478()
    Case 1479: MC4 = M1479()
    Case 1480: MC4 = M1480()
    Case 1481: MC4 = M1481()
    Case 1482: MC4 = M1482()
    Case 1483: MC4 = M1483()
    Case 1484: MC4 = M1484()
    Case 1485: MC4 = M1485()
    Case 1486: MC4 = M1486()
    Case 1487: MC4 = M1487()
    Case 1488: MC4 = M1488()
    Case 1489: MC4 = M1489()
    Case 1490: MC4 = M1490()
    Case 1491: MC4 = M1491()
    Case 1492: MC4 = M1492()
    Case 1493: MC4 = M1493()
    Case 1494: MC4 = M1494()
    Case 1495: MC4 = M1495()
    Case 1496: MC4 = M1496()
    Case 1497: MC4 = M1497()
    Case 1498: MC4 = M1498()
    Case 1499: MC4 = M1499()
    End Select
    Exit Function
EH:
    MC4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1500: MC5 = M1500()
    Case 1501: MC5 = M1501()
    Case 1502: MC5 = M1502()
    Case 1503: MC5 = M1503()
    Case 1504: MC5 = M1504()
    Case 1505: MC5 = M1505()
    Case 1506: MC5 = M1506()
    Case 1507: MC5 = M1507()
    Case 1508: MC5 = M1508()
    Case 1509: MC5 = M1509()
    Case 1510: MC5 = M1510()
    Case 1511: MC5 = M1511()
    Case 1512: MC5 = M1512()
    Case 1513: MC5 = M1513()
    Case 1514: MC5 = M1514()
    Case 1515: MC5 = M1515()
    Case 1516: MC5 = M1516()
    Case 1517: MC5 = M1517()
    Case 1518: MC5 = M1518()
    Case 1519: MC5 = M1519()
    Case 1520: MC5 = M1520()
    Case 1521: MC5 = M1521()
    Case 1522: MC5 = M1522()
    Case 1523: MC5 = M1523()
    Case 1524: MC5 = M1524()
    Case 1525: MC5 = M1525()
    Case 1526: MC5 = M1526()
    Case 1527: MC5 = M1527()
    Case 1528: MC5 = M1528()
    Case 1529: MC5 = M1529()
    Case 1530: MC5 = M1530()
    Case 1531: MC5 = M1531()
    Case 1532: MC5 = M1532()
    Case 1533: MC5 = M1533()
    Case 1534: MC5 = M1534()
    Case 1535: MC5 = M1535()
    Case 1536: MC5 = M1536()
    Case 1537: MC5 = M1537()
    Case 1538: MC5 = M1538()
    Case 1539: MC5 = M1539()
    Case 1540: MC5 = M1540()
    Case 1541: MC5 = M1541()
    Case 1542: MC5 = M1542()
    Case 1543: MC5 = M1543()
    Case 1544: MC5 = M1544()
    Case 1545: MC5 = M1545()
    Case 1546: MC5 = M1546()
    Case 1547: MC5 = M1547()
    Case 1548: MC5 = M1548()
    Case 1549: MC5 = M1549()
    Case 1550: MC5 = M1550()
    Case 1551: MC5 = M1551()
    Case 1552: MC5 = M1552()
    Case 1553: MC5 = M1553()
    Case 1554: MC5 = M1554()
    Case 1555: MC5 = M1555()
    Case 1556: MC5 = M1556()
    Case 1557: MC5 = M1557()
    Case 1558: MC5 = M1558()
    Case 1559: MC5 = M1559()
    Case 1560: MC5 = M1560()
    Case 1561: MC5 = M1561()
    Case 1562: MC5 = M1562()
    Case 1563: MC5 = M1563()
    Case 1564: MC5 = M1564()
    Case 1565: MC5 = M1565()
    Case 1566: MC5 = M1566()
    Case 1567: MC5 = M1567()
    Case 1568: MC5 = M1568()
    Case 1569: MC5 = M1569()
    Case 1570: MC5 = M1570()
    Case 1571: MC5 = M1571()
    Case 1572: MC5 = M1572()
    Case 1573: MC5 = M1573()
    Case 1574: MC5 = M1574()
    Case 1575: MC5 = M1575()
    Case 1576: MC5 = M1576()
    Case 1577: MC5 = M1577()
    Case 1578: MC5 = M1578()
    Case 1579: MC5 = M1579()
    Case 1580: MC5 = M1580()
    Case 1581: MC5 = M1581()
    Case 1582: MC5 = M1582()
    Case 1583: MC5 = M1583()
    Case 1584: MC5 = M1584()
    Case 1585: MC5 = M1585()
    Case 1586: MC5 = M1586()
    Case 1587: MC5 = M1587()
    Case 1588: MC5 = M1588()
    Case 1589: MC5 = M1589()
    Case 1590: MC5 = M1590()
    Case 1591: MC5 = M1591()
    Case 1592: MC5 = M1592()
    Case 1593: MC5 = M1593()
    Case 1594: MC5 = M1594()
    Case 1595: MC5 = M1595()
    Case 1596: MC5 = M1596()
    Case 1597: MC5 = M1597()
    Case 1598: MC5 = M1598()
    Case 1599: MC5 = M1599()
    End Select
    Exit Function
EH:
    MC5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC6(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1600: MC6 = M1600()
    Case 1601: MC6 = M1601()
    Case 1602: MC6 = M1602()
    Case 1603: MC6 = M1603()
    Case 1604: MC6 = M1604()
    Case 1605: MC6 = M1605()
    Case 1606: MC6 = M1606()
    Case 1607: MC6 = M1607()
    Case 1608: MC6 = M1608()
    Case 1609: MC6 = M1609()
    Case 1610: MC6 = M1610()
    Case 1611: MC6 = M1611()
    Case 1612: MC6 = M1612()
    Case 1613: MC6 = M1613()
    Case 1614: MC6 = M1614()
    Case 1615: MC6 = M1615()
    Case 1616: MC6 = M1616()
    Case 1617: MC6 = M1617()
    Case 1618: MC6 = M1618()
    Case 1619: MC6 = M1619()
    Case 1620: MC6 = M1620()
    Case 1621: MC6 = M1621()
    Case 1622: MC6 = M1622()
    Case 1623: MC6 = M1623()
    Case 1624: MC6 = M1624()
    Case 1625: MC6 = M1625()
    Case 1626: MC6 = M1626()
    Case 1627: MC6 = M1627()
    Case 1628: MC6 = M1628()
    Case 1629: MC6 = M1629()
    Case 1630: MC6 = M1630()
    Case 1631: MC6 = M1631()
    Case 1632: MC6 = M1632()
    Case 1633: MC6 = M1633()
    Case 1634: MC6 = M1634()
    Case 1635: MC6 = M1635()
    Case 1636: MC6 = M1636()
    Case 1637: MC6 = M1637()
    Case 1638: MC6 = M1638()
    Case 1639: MC6 = M1639()
    Case 1640: MC6 = M1640()
    Case 1641: MC6 = M1641()
    Case 1642: MC6 = M1642()
    Case 1643: MC6 = M1643()
    Case 1644: MC6 = M1644()
    Case 1645: MC6 = M1645()
    Case 1646: MC6 = M1646()
    Case 1647: MC6 = M1647()
    Case 1648: MC6 = M1648()
    Case 1649: MC6 = M1649()
    Case 1650: MC6 = M1650()
    Case 1651: MC6 = M1651()
    Case 1652: MC6 = M1652()
    Case 1653: MC6 = M1653()
    Case 1654: MC6 = M1654()
    Case 1655: MC6 = M1655()
    Case 1656: MC6 = M1656()
    Case 1657: MC6 = M1657()
    Case 1658: MC6 = M1658()
    Case 1659: MC6 = M1659()
    Case 1660: MC6 = M1660()
    Case 1661: MC6 = M1661()
    Case 1662: MC6 = M1662()
    Case 1663: MC6 = M1663()
    Case 1664: MC6 = M1664()
    Case 1665: MC6 = M1665()
    Case 1666: MC6 = M1666()
    Case 1667: MC6 = M1667()
    Case 1668: MC6 = M1668()
    Case 1669: MC6 = M1669()
    Case 1670: MC6 = M1670()
    Case 1671: MC6 = M1671()
    Case 1672: MC6 = M1672()
    Case 1673: MC6 = M1673()
    Case 1674: MC6 = M1674()
    Case 1675: MC6 = M1675()
    Case 1676: MC6 = M1676()
    Case 1677: MC6 = M1677()
    Case 1678: MC6 = M1678()
    Case 1679: MC6 = M1679()
    Case 1680: MC6 = M1680()
    Case 1681: MC6 = M1681()
    Case 1682: MC6 = M1682()
    Case 1683: MC6 = M1683()
    Case 1684: MC6 = M1684()
    Case 1685: MC6 = M1685()
    Case 1686: MC6 = M1686()
    Case 1687: MC6 = M1687()
    Case 1688: MC6 = M1688()
    Case 1689: MC6 = M1689()
    Case 1690: MC6 = M1690()
    Case 1691: MC6 = M1691()
    Case 1692: MC6 = M1692()
    Case 1693: MC6 = M1693()
    Case 1694: MC6 = M1694()
    Case 1695: MC6 = M1695()
    Case 1696: MC6 = M1696()
    Case 1697: MC6 = M1697()
    Case 1698: MC6 = M1698()
    Case 1699: MC6 = M1699()
    End Select
    Exit Function
EH:
    MC6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC7(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1700: MC7 = M1700()
    Case 1701: MC7 = M1701()
    Case 1702: MC7 = M1702()
    Case 1703: MC7 = M1703()
    Case 1704: MC7 = M1704()
    Case 1705: MC7 = M1705()
    Case 1706: MC7 = M1706()
    Case 1707: MC7 = M1707()
    Case 1708: MC7 = M1708()
    Case 1709: MC7 = M1709()
    Case 1710: MC7 = M1710()
    Case 1711: MC7 = M1711()
    Case 1712: MC7 = M1712()
    Case 1713: MC7 = M1713()
    Case 1714: MC7 = M1714()
    Case 1715: MC7 = M1715()
    Case 1716: MC7 = M1716()
    Case 1717: MC7 = M1717()
    Case 1718: MC7 = M1718()
    Case 1719: MC7 = M1719()
    Case 1720: MC7 = M1720()
    Case 1721: MC7 = M1721()
    Case 1722: MC7 = M1722()
    Case 1723: MC7 = M1723()
    Case 1724: MC7 = M1724()
    Case 1725: MC7 = M1725()
    Case 1726: MC7 = M1726()
    Case 1727: MC7 = M1727()
    Case 1728: MC7 = M1728()
    Case 1729: MC7 = M1729()
    Case 1730: MC7 = M1730()
    Case 1731: MC7 = M1731()
    Case 1732: MC7 = M1732()
    Case 1733: MC7 = M1733()
    Case 1734: MC7 = M1734()
    Case 1735: MC7 = M1735()
    Case 1736: MC7 = M1736()
    Case 1737: MC7 = M1737()
    Case 1738: MC7 = M1738()
    Case 1739: MC7 = M1739()
    Case 1740: MC7 = M1740()
    Case 1741: MC7 = M1741()
    Case 1742: MC7 = M1742()
    Case 1743: MC7 = M1743()
    Case 1744: MC7 = M1744()
    Case 1745: MC7 = M1745()
    Case 1746: MC7 = M1746()
    Case 1747: MC7 = M1747()
    Case 1748: MC7 = M1748()
    Case 1749: MC7 = M1749()
    Case 1750: MC7 = M1750()
    Case 1751: MC7 = M1751()
    Case 1752: MC7 = M1752()
    Case 1753: MC7 = M1753()
    Case 1754: MC7 = M1754()
    Case 1755: MC7 = M1755()
    Case 1756: MC7 = M1756()
    Case 1757: MC7 = M1757()
    Case 1758: MC7 = M1758()
    Case 1759: MC7 = M1759()
    Case 1760: MC7 = M1760()
    Case 1761: MC7 = M1761()
    Case 1762: MC7 = M1762()
    Case 1763: MC7 = M1763()
    Case 1764: MC7 = M1764()
    Case 1765: MC7 = M1765()
    Case 1766: MC7 = M1766()
    Case 1767: MC7 = M1767()
    Case 1768: MC7 = M1768()
    Case 1769: MC7 = M1769()
    Case 1770: MC7 = M1770()
    Case 1771: MC7 = M1771()
    Case 1772: MC7 = M1772()
    Case 1773: MC7 = M1773()
    Case 1774: MC7 = M1774()
    Case 1775: MC7 = M1775()
    Case 1776: MC7 = M1776()
    Case 1777: MC7 = M1777()
    Case 1778: MC7 = M1778()
    Case 1779: MC7 = M1779()
    Case 1780: MC7 = M1780()
    Case 1781: MC7 = M1781()
    Case 1782: MC7 = M1782()
    Case 1783: MC7 = M1783()
    Case 1784: MC7 = M1784()
    Case 1785: MC7 = M1785()
    Case 1786: MC7 = M1786()
    Case 1787: MC7 = M1787()
    Case 1788: MC7 = M1788()
    Case 1789: MC7 = M1789()
    Case 1790: MC7 = M1790()
    Case 1791: MC7 = M1791()
    Case 1792: MC7 = M1792()
    Case 1793: MC7 = M1793()
    Case 1794: MC7 = M1794()
    Case 1795: MC7 = M1795()
    Case 1796: MC7 = M1796()
    Case 1797: MC7 = M1797()
    Case 1798: MC7 = M1798()
    Case 1799: MC7 = M1799()
    End Select
    Exit Function
EH:
    MC7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC8(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1800: MC8 = M1800()
    Case 1801: MC8 = M1801()
    Case 1802: MC8 = M1802()
    Case 1803: MC8 = M1803()
    Case 1804: MC8 = M1804()
    Case 1805: MC8 = M1805()
    Case 1806: MC8 = M1806()
    Case 1807: MC8 = M1807()
    Case 1808: MC8 = M1808()
    Case 1809: MC8 = M1809()
    Case 1810: MC8 = M1810()
    Case 1811: MC8 = M1811()
    Case 1812: MC8 = M1812()
    Case 1813: MC8 = M1813()
    Case 1814: MC8 = M1814()
    Case 1815: MC8 = M1815()
    Case 1816: MC8 = M1816()
    Case 1817: MC8 = M1817()
    Case 1818: MC8 = M1818()
    Case 1819: MC8 = M1819()
    Case 1820: MC8 = M1820()
    Case 1821: MC8 = M1821()
    Case 1822: MC8 = M1822()
    Case 1823: MC8 = M1823()
    Case 1824: MC8 = M1824()
    Case 1825: MC8 = M1825()
    Case 1826: MC8 = M1826()
    Case 1827: MC8 = M1827()
    Case 1828: MC8 = M1828()
    Case 1829: MC8 = M1829()
    Case 1830: MC8 = M1830()
    Case 1831: MC8 = M1831()
    Case 1832: MC8 = M1832()
    Case 1833: MC8 = M1833()
    Case 1834: MC8 = M1834()
    Case 1835: MC8 = M1835()
    Case 1836: MC8 = M1836()
    Case 1837: MC8 = M1837()
    Case 1838: MC8 = M1838()
    Case 1839: MC8 = M1839()
    Case 1840: MC8 = M1840()
    Case 1841: MC8 = M1841()
    Case 1842: MC8 = M1842()
    Case 1843: MC8 = M1843()
    Case 1844: MC8 = M1844()
    Case 1845: MC8 = M1845()
    Case 1846: MC8 = M1846()
    Case 1847: MC8 = M1847()
    Case 1848: MC8 = M1848()
    Case 1849: MC8 = M1849()
    Case 1850: MC8 = M1850()
    Case 1851: MC8 = M1851()
    Case 1852: MC8 = M1852()
    Case 1853: MC8 = M1853()
    Case 1854: MC8 = M1854()
    Case 1855: MC8 = M1855()
    Case 1856: MC8 = M1856()
    Case 1857: MC8 = M1857()
    Case 1858: MC8 = M1858()
    Case 1859: MC8 = M1859()
    Case 1860: MC8 = M1860()
    Case 1861: MC8 = M1861()
    Case 1862: MC8 = M1862()
    Case 1863: MC8 = M1863()
    Case 1864: MC8 = M1864()
    Case 1865: MC8 = M1865()
    Case 1866: MC8 = M1866()
    Case 1867: MC8 = M1867()
    Case 1868: MC8 = M1868()
    Case 1869: MC8 = M1869()
    Case 1870: MC8 = M1870()
    Case 1871: MC8 = M1871()
    Case 1872: MC8 = M1872()
    Case 1873: MC8 = M1873()
    Case 1874: MC8 = M1874()
    Case 1875: MC8 = M1875()
    Case 1876: MC8 = M1876()
    Case 1877: MC8 = M1877()
    Case 1878: MC8 = M1878()
    Case 1879: MC8 = M1879()
    Case 1880: MC8 = M1880()
    Case 1881: MC8 = M1881()
    Case 1882: MC8 = M1882()
    Case 1883: MC8 = M1883()
    Case 1884: MC8 = M1884()
    Case 1885: MC8 = M1885()
    Case 1886: MC8 = M1886()
    Case 1887: MC8 = M1887()
    Case 1888: MC8 = M1888()
    Case 1889: MC8 = M1889()
    Case 1890: MC8 = M1890()
    Case 1891: MC8 = M1891()
    Case 1892: MC8 = M1892()
    Case 1893: MC8 = M1893()
    Case 1894: MC8 = M1894()
    Case 1895: MC8 = M1895()
    Case 1896: MC8 = M1896()
    Case 1897: MC8 = M1897()
    Case 1898: MC8 = M1898()
    Case 1899: MC8 = M1899()
    End Select
    Exit Function
EH:
    MC8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC9(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 1900: MC9 = M1900()
    Case 1901: MC9 = M1901()
    Case 1902: MC9 = M1902()
    Case 1903: MC9 = M1903()
    Case 1904: MC9 = M1904()
    Case 1905: MC9 = M1905()
    Case 1906: MC9 = M1906()
    Case 1907: MC9 = M1907()
    Case 1908: MC9 = M1908()
    Case 1909: MC9 = M1909()
    Case 1910: MC9 = M1910()
    Case 1911: MC9 = M1911()
    Case 1912: MC9 = M1912()
    Case 1913: MC9 = M1913()
    Case 1914: MC9 = M1914()
    Case 1915: MC9 = M1915()
    Case 1916: MC9 = M1916()
    Case 1917: MC9 = M1917()
    Case 1918: MC9 = M1918()
    Case 1919: MC9 = M1919()
    Case 1920: MC9 = M1920()
    Case 1921: MC9 = M1921()
    Case 1922: MC9 = M1922()
    Case 1923: MC9 = M1923()
    Case 1924: MC9 = M1924()
    Case 1925: MC9 = M1925()
    Case 1926: MC9 = M1926()
    Case 1927: MC9 = M1927()
    Case 1928: MC9 = M1928()
    Case 1929: MC9 = M1929()
    Case 1930: MC9 = M1930()
    Case 1931: MC9 = M1931()
    Case 1932: MC9 = M1932()
    Case 1933: MC9 = M1933()
    Case 1934: MC9 = M1934()
    Case 1935: MC9 = M1935()
    Case 1936: MC9 = M1936()
    Case 1937: MC9 = M1937()
    Case 1938: MC9 = M1938()
    Case 1939: MC9 = M1939()
    Case 1940: MC9 = M1940()
    Case 1941: MC9 = M1941()
    Case 1942: MC9 = M1942()
    Case 1943: MC9 = M1943()
    Case 1944: MC9 = M1944()
    Case 1945: MC9 = M1945()
    Case 1946: MC9 = M1946()
    Case 1947: MC9 = M1947()
    Case 1948: MC9 = M1948()
    Case 1949: MC9 = M1949()
    Case 1950: MC9 = M1950()
    Case 1951: MC9 = M1951()
    Case 1952: MC9 = M1952()
    Case 1953: MC9 = M1953()
    Case 1954: MC9 = M1954()
    Case 1955: MC9 = M1955()
    Case 1956: MC9 = M1956()
    Case 1957: MC9 = M1957()
    Case 1958: MC9 = M1958()
    Case 1959: MC9 = M1959()
    Case 1960: MC9 = M1960()
    Case 1961: MC9 = M1961()
    Case 1962: MC9 = M1962()
    Case 1963: MC9 = M1963()
    Case 1964: MC9 = M1964()
    Case 1965: MC9 = M1965()
    Case 1966: MC9 = M1966()
    Case 1967: MC9 = M1967()
    Case 1968: MC9 = M1968()
    Case 1969: MC9 = M1969()
    Case 1970: MC9 = M1970()
    Case 1971: MC9 = M1971()
    Case 1972: MC9 = M1972()
    Case 1973: MC9 = M1973()
    Case 1974: MC9 = M1974()
    Case 1975: MC9 = M1975()
    Case 1976: MC9 = M1976()
    Case 1977: MC9 = M1977()
    Case 1978: MC9 = M1978()
    Case 1979: MC9 = M1979()
    Case 1980: MC9 = M1980()
    Case 1981: MC9 = M1981()
    Case 1982: MC9 = M1982()
    Case 1983: MC9 = M1983()
    Case 1984: MC9 = M1984()
    Case 1985: MC9 = M1985()
    Case 1986: MC9 = M1986()
    Case 1987: MC9 = M1987()
    Case 1988: MC9 = M1988()
    Case 1989: MC9 = M1989()
    Case 1990: MC9 = M1990()
    Case 1991: MC9 = M1991()
    Case 1992: MC9 = M1992()
    Case 1993: MC9 = M1993()
    Case 1994: MC9 = M1994()
    Case 1995: MC9 = M1995()
    Case 1996: MC9 = M1996()
    Case 1997: MC9 = M1997()
    Case 1998: MC9 = M1998()
    Case 1999: MC9 = M1999()
    End Select
    Exit Function
EH:
    MC9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1000() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 + 1))
    M1000 = R
    Exit Function
EH:
    M1000 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1001() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 & "a"))
    M1001 = R
    Exit Function
EH:
    M1001 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1002() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5))
    M1002 = R
    Exit Function
EH:
    M1002 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1003() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 5))
    M1003 = R
    Exit Function
EH:
    M1003 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1004() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 And 3))
    M1004 = R
    Exit Function
EH:
    M1004 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1005() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 = 0))
    M1005 = R
    Exit Function
EH:
    M1005 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1006() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 \ 2))
    M1006 = R
    Exit Function
EH:
    M1006 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1007() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Mod 2))
    M1007 = R
    Exit Function
EH:
    M1007 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1008() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 ^ 2))
    M1008 = R
    Exit Function
EH:
    M1008 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1009() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(-5)))
    M1009 = R
    Exit Function
EH:
    M1009 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1010() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(-5)))
    M1010 = R
    Exit Function
EH:
    M1010 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1011() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(-5)))
    M1011 = R
    Exit Function
EH:
    M1011 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1012() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-5)))
    M1012 = R
    Exit Function
EH:
    M1012 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1013() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(-5)))
    M1013 = R
    Exit Function
EH:
    M1013 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1014() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 + 1))
    M1014 = R
    Exit Function
EH:
    M1014 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1015() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 & "a"))
    M1015 = R
    Exit Function
EH:
    M1015 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1016() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(--5))
    M1016 = R
    Exit Function
EH:
    M1016 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1017() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not -5))
    M1017 = R
    Exit Function
EH:
    M1017 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1018() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 And 3))
    M1018 = R
    Exit Function
EH:
    M1018 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1019() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 = 0))
    M1019 = R
    Exit Function
EH:
    M1019 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1020() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 \ 2))
    M1020 = R
    Exit Function
EH:
    M1020 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1021() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 Mod 2))
    M1021 = R
    Exit Function
EH:
    M1021 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1022() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-5 ^ 2))
    M1022 = R
    Exit Function
EH:
    M1022 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1023() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(1.5)))
    M1023 = R
    Exit Function
EH:
    M1023 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1024() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(1.5)))
    M1024 = R
    Exit Function
EH:
    M1024 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1025() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(1.5)))
    M1025 = R
    Exit Function
EH:
    M1025 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1026() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(1.5)))
    M1026 = R
    Exit Function
EH:
    M1026 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1027() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(1.5)))
    M1027 = R
    Exit Function
EH:
    M1027 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1028() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 + 1))
    M1028 = R
    Exit Function
EH:
    M1028 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1029() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 & "a"))
    M1029 = R
    Exit Function
EH:
    M1029 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1030() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1.5))
    M1030 = R
    Exit Function
EH:
    M1030 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1031() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 1.5))
    M1031 = R
    Exit Function
EH:
    M1031 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1032() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 And 3))
    M1032 = R
    Exit Function
EH:
    M1032 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1033() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 = 0))
    M1033 = R
    Exit Function
EH:
    M1033 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1034() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 \ 2))
    M1034 = R
    Exit Function
EH:
    M1034 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1035() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 Mod 2))
    M1035 = R
    Exit Function
EH:
    M1035 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1036() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5 ^ 2))
    M1036 = R
    Exit Function
EH:
    M1036 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1037() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(0.5)))
    M1037 = R
    Exit Function
EH:
    M1037 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1038() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(0.5)))
    M1038 = R
    Exit Function
EH:
    M1038 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1039() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(0.5)))
    M1039 = R
    Exit Function
EH:
    M1039 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1040() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(0.5)))
    M1040 = R
    Exit Function
EH:
    M1040 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1041() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(0.5)))
    M1041 = R
    Exit Function
EH:
    M1041 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1042() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 + 1))
    M1042 = R
    Exit Function
EH:
    M1042 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1043() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 & "a"))
    M1043 = R
    Exit Function
EH:
    M1043 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1044() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5))
    M1044 = R
    Exit Function
EH:
    M1044 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1045() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 0.5))
    M1045 = R
    Exit Function
EH:
    M1045 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1046() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 And 3))
    M1046 = R
    Exit Function
EH:
    M1046 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1047() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 = 0))
    M1047 = R
    Exit Function
EH:
    M1047 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1048() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 \ 2))
    M1048 = R
    Exit Function
EH:
    M1048 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1049() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 Mod 2))
    M1049 = R
    Exit Function
EH:
    M1049 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1050() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.5 ^ 2))
    M1050 = R
    Exit Function
EH:
    M1050 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1051() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(-0.5)))
    M1051 = R
    Exit Function
EH:
    M1051 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1052() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(-0.5)))
    M1052 = R
    Exit Function
EH:
    M1052 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1053() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(-0.5)))
    M1053 = R
    Exit Function
EH:
    M1053 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1054() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-0.5)))
    M1054 = R
    Exit Function
EH:
    M1054 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1055() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(-0.5)))
    M1055 = R
    Exit Function
EH:
    M1055 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1056() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 + 1))
    M1056 = R
    Exit Function
EH:
    M1056 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1057() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 & "a"))
    M1057 = R
    Exit Function
EH:
    M1057 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1058() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(--0.5))
    M1058 = R
    Exit Function
EH:
    M1058 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1059() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not -0.5))
    M1059 = R
    Exit Function
EH:
    M1059 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1060() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 And 3))
    M1060 = R
    Exit Function
EH:
    M1060 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1061() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 = 0))
    M1061 = R
    Exit Function
EH:
    M1061 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1062() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 \ 2))
    M1062 = R
    Exit Function
EH:
    M1062 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1063() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 Mod 2))
    M1063 = R
    Exit Function
EH:
    M1063 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1064() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-0.5 ^ 2))
    M1064 = R
    Exit Function
EH:
    M1064 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1065() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(1E15)))
    M1065 = R
    Exit Function
EH:
    M1065 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1066() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(1E15)))
    M1066 = R
    Exit Function
EH:
    M1066 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1067() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(1E15)))
    M1067 = R
    Exit Function
EH:
    M1067 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1068() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(1E15)))
    M1068 = R
    Exit Function
EH:
    M1068 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1069() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(1E15)))
    M1069 = R
    Exit Function
EH:
    M1069 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1070() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 + 1))
    M1070 = R
    Exit Function
EH:
    M1070 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1071() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 & "a"))
    M1071 = R
    Exit Function
EH:
    M1071 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1072() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1E15))
    M1072 = R
    Exit Function
EH:
    M1072 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1073() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 1E15))
    M1073 = R
    Exit Function
EH:
    M1073 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1074() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 And 3))
    M1074 = R
    Exit Function
EH:
    M1074 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1075() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 = 0))
    M1075 = R
    Exit Function
EH:
    M1075 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1076() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 \ 2))
    M1076 = R
    Exit Function
EH:
    M1076 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1077() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 Mod 2))
    M1077 = R
    Exit Function
EH:
    M1077 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1078() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E15 ^ 2))
    M1078 = R
    Exit Function
EH:
    M1078 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1079() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(1E-5)))
    M1079 = R
    Exit Function
EH:
    M1079 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1080() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(1E-5)))
    M1080 = R
    Exit Function
EH:
    M1080 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1081() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(1E-5)))
    M1081 = R
    Exit Function
EH:
    M1081 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1082() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(1E-5)))
    M1082 = R
    Exit Function
EH:
    M1082 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1083() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(1E-5)))
    M1083 = R
    Exit Function
EH:
    M1083 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1084() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 + 1))
    M1084 = R
    Exit Function
EH:
    M1084 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1085() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 & "a"))
    M1085 = R
    Exit Function
EH:
    M1085 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1086() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1E-5))
    M1086 = R
    Exit Function
EH:
    M1086 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1087() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 1E-5))
    M1087 = R
    Exit Function
EH:
    M1087 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1088() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 And 3))
    M1088 = R
    Exit Function
EH:
    M1088 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1089() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 = 0))
    M1089 = R
    Exit Function
EH:
    M1089 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1090() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 \ 2))
    M1090 = R
    Exit Function
EH:
    M1090 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1091() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 Mod 2))
    M1091 = R
    Exit Function
EH:
    M1091 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1092() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E-5 ^ 2))
    M1092 = R
    Exit Function
EH:
    M1092 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1093() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(True)))
    M1093 = R
    Exit Function
EH:
    M1093 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1094() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(True)))
    M1094 = R
    Exit Function
EH:
    M1094 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1095() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(True)))
    M1095 = R
    Exit Function
EH:
    M1095 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1096() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(True)))
    M1096 = R
    Exit Function
EH:
    M1096 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1097() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(True)))
    M1097 = R
    Exit Function
EH:
    M1097 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1098() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True + 1))
    M1098 = R
    Exit Function
EH:
    M1098 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1099() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True & "a"))
    M1099 = R
    Exit Function
EH:
    M1099 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1100() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-True))
    M1100 = R
    Exit Function
EH:
    M1100 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1101() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not True))
    M1101 = R
    Exit Function
EH:
    M1101 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1102() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True And 3))
    M1102 = R
    Exit Function
EH:
    M1102 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1103() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True = 0))
    M1103 = R
    Exit Function
EH:
    M1103 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1104() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True \ 2))
    M1104 = R
    Exit Function
EH:
    M1104 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1105() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True Mod 2))
    M1105 = R
    Exit Function
EH:
    M1105 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1106() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True ^ 2))
    M1106 = R
    Exit Function
EH:
    M1106 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1107() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(False)))
    M1107 = R
    Exit Function
EH:
    M1107 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1108() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(False)))
    M1108 = R
    Exit Function
EH:
    M1108 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1109() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(False)))
    M1109 = R
    Exit Function
EH:
    M1109 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1110() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(False)))
    M1110 = R
    Exit Function
EH:
    M1110 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1111() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(False)))
    M1111 = R
    Exit Function
EH:
    M1111 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1112() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False + 1))
    M1112 = R
    Exit Function
EH:
    M1112 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1113() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False & "a"))
    M1113 = R
    Exit Function
EH:
    M1113 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1114() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-False))
    M1114 = R
    Exit Function
EH:
    M1114 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1115() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not False))
    M1115 = R
    Exit Function
EH:
    M1115 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1116() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False And 3))
    M1116 = R
    Exit Function
EH:
    M1116 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1117() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False = 0))
    M1117 = R
    Exit Function
EH:
    M1117 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1118() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False \ 2))
    M1118 = R
    Exit Function
EH:
    M1118 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1119() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False Mod 2))
    M1119 = R
    Exit Function
EH:
    M1119 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1120() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(False ^ 2))
    M1120 = R
    Exit Function
EH:
    M1120 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1121() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(Null)))
    M1121 = R
    Exit Function
EH:
    M1121 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1122() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(Null)))
    M1122 = R
    Exit Function
EH:
    M1122 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1123() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(Null)))
    M1123 = R
    Exit Function
EH:
    M1123 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1124() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(Null)))
    M1124 = R
    Exit Function
EH:
    M1124 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1125() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(Null)))
    M1125 = R
    Exit Function
EH:
    M1125 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1126() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null + 1))
    M1126 = R
    Exit Function
EH:
    M1126 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1127() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null & "a"))
    M1127 = R
    Exit Function
EH:
    M1127 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1128() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-Null))
    M1128 = R
    Exit Function
EH:
    M1128 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1129() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not Null))
    M1129 = R
    Exit Function
EH:
    M1129 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1130() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null And 3))
    M1130 = R
    Exit Function
EH:
    M1130 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1131() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null = 0))
    M1131 = R
    Exit Function
EH:
    M1131 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1132() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null \ 2))
    M1132 = R
    Exit Function
EH:
    M1132 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1133() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null Mod 2))
    M1133 = R
    Exit Function
EH:
    M1133 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1134() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null ^ 2))
    M1134 = R
    Exit Function
EH:
    M1134 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1135() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(Empty)))
    M1135 = R
    Exit Function
EH:
    M1135 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1136() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(Empty)))
    M1136 = R
    Exit Function
EH:
    M1136 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1137() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(Empty)))
    M1137 = R
    Exit Function
EH:
    M1137 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1138() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(Empty)))
    M1138 = R
    Exit Function
EH:
    M1138 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1139() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(Empty)))
    M1139 = R
    Exit Function
EH:
    M1139 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1140() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty + 1))
    M1140 = R
    Exit Function
EH:
    M1140 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1141() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty & "a"))
    M1141 = R
    Exit Function
EH:
    M1141 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1142() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-Empty))
    M1142 = R
    Exit Function
EH:
    M1142 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1143() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not Empty))
    M1143 = R
    Exit Function
EH:
    M1143 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1144() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty And 3))
    M1144 = R
    Exit Function
EH:
    M1144 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1145() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty = 0))
    M1145 = R
    Exit Function
EH:
    M1145 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1146() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty \ 2))
    M1146 = R
    Exit Function
EH:
    M1146 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1147() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty Mod 2))
    M1147 = R
    Exit Function
EH:
    M1147 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1148() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty ^ 2))
    M1148 = R
    Exit Function
EH:
    M1148 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1149() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str("5")))
    M1149 = R
    Exit Function
EH:
    M1149 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1150() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr("5")))
    M1150 = R
    Exit Function
EH:
    M1150 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1151() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val("5")))
    M1151 = R
    Exit Function
EH:
    M1151 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1152() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("5")))
    M1152 = R
    Exit Function
EH:
    M1152 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1153() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("5")))
    M1153 = R
    Exit Function
EH:
    M1153 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1154() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" + 1))
    M1154 = R
    Exit Function
EH:
    M1154 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1155() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" & "a"))
    M1155 = R
    Exit Function
EH:
    M1155 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1156() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-"5"))
    M1156 = R
    Exit Function
EH:
    M1156 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1157() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not "5"))
    M1157 = R
    Exit Function
EH:
    M1157 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1158() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" And 3))
    M1158 = R
    Exit Function
EH:
    M1158 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1159() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" = 0))
    M1159 = R
    Exit Function
EH:
    M1159 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1160() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" \ 2))
    M1160 = R
    Exit Function
EH:
    M1160 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1161() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" Mod 2))
    M1161 = R
    Exit Function
EH:
    M1161 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1162() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" ^ 2))
    M1162 = R
    Exit Function
EH:
    M1162 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1163() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str("-5")))
    M1163 = R
    Exit Function
EH:
    M1163 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1164() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr("-5")))
    M1164 = R
    Exit Function
EH:
    M1164 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1165() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val("-5")))
    M1165 = R
    Exit Function
EH:
    M1165 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1166() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("-5")))
    M1166 = R
    Exit Function
EH:
    M1166 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1167() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("-5")))
    M1167 = R
    Exit Function
EH:
    M1167 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1168() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" + 1))
    M1168 = R
    Exit Function
EH:
    M1168 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1169() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" & "a"))
    M1169 = R
    Exit Function
EH:
    M1169 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1170() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-"-5"))
    M1170 = R
    Exit Function
EH:
    M1170 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1171() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not "-5"))
    M1171 = R
    Exit Function
EH:
    M1171 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1172() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" And 3))
    M1172 = R
    Exit Function
EH:
    M1172 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1173() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" = 0))
    M1173 = R
    Exit Function
EH:
    M1173 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1174() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" \ 2))
    M1174 = R
    Exit Function
EH:
    M1174 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1175() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" Mod 2))
    M1175 = R
    Exit Function
EH:
    M1175 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1176() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("-5" ^ 2))
    M1176 = R
    Exit Function
EH:
    M1176 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1177() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(" 5")))
    M1177 = R
    Exit Function
EH:
    M1177 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1178() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(" 5")))
    M1178 = R
    Exit Function
EH:
    M1178 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1179() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(" 5")))
    M1179 = R
    Exit Function
EH:
    M1179 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1180() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(" 5")))
    M1180 = R
    Exit Function
EH:
    M1180 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1181() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(" 5")))
    M1181 = R
    Exit Function
EH:
    M1181 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1182() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" + 1))
    M1182 = R
    Exit Function
EH:
    M1182 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1183() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" & "a"))
    M1183 = R
    Exit Function
EH:
    M1183 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1184() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-" 5"))
    M1184 = R
    Exit Function
EH:
    M1184 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1185() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not " 5"))
    M1185 = R
    Exit Function
EH:
    M1185 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1186() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" And 3))
    M1186 = R
    Exit Function
EH:
    M1186 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1187() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" = 0))
    M1187 = R
    Exit Function
EH:
    M1187 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1188() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" \ 2))
    M1188 = R
    Exit Function
EH:
    M1188 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1189() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" Mod 2))
    M1189 = R
    Exit Function
EH:
    M1189 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1190() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(" 5" ^ 2))
    M1190 = R
    Exit Function
EH:
    M1190 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1191() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str("abc")))
    M1191 = R
    Exit Function
EH:
    M1191 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1192() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr("abc")))
    M1192 = R
    Exit Function
EH:
    M1192 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1193() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val("abc")))
    M1193 = R
    Exit Function
EH:
    M1193 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1194() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("abc")))
    M1194 = R
    Exit Function
EH:
    M1194 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1195() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("abc")))
    M1195 = R
    Exit Function
EH:
    M1195 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1196() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" + 1))
    M1196 = R
    Exit Function
EH:
    M1196 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1197() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" & "a"))
    M1197 = R
    Exit Function
EH:
    M1197 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1198() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-"abc"))
    M1198 = R
    Exit Function
EH:
    M1198 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1199() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not "abc"))
    M1199 = R
    Exit Function
EH:
    M1199 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1200() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" And 3))
    M1200 = R
    Exit Function
EH:
    M1200 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1201() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" = 0))
    M1201 = R
    Exit Function
EH:
    M1201 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1202() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" \ 2))
    M1202 = R
    Exit Function
EH:
    M1202 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1203() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" Mod 2))
    M1203 = R
    Exit Function
EH:
    M1203 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1204() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" ^ 2))
    M1204 = R
    Exit Function
EH:
    M1204 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1205() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str("")))
    M1205 = R
    Exit Function
EH:
    M1205 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1206() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr("")))
    M1206 = R
    Exit Function
EH:
    M1206 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1207() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val("")))
    M1207 = R
    Exit Function
EH:
    M1207 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1208() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("")))
    M1208 = R
    Exit Function
EH:
    M1208 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1209() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("")))
    M1209 = R
    Exit Function
EH:
    M1209 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1210() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" + 1))
    M1210 = R
    Exit Function
EH:
    M1210 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1211() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" & "a"))
    M1211 = R
    Exit Function
EH:
    M1211 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1212() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-""))
    M1212 = R
    Exit Function
EH:
    M1212 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1213() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not ""))
    M1213 = R
    Exit Function
EH:
    M1213 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1214() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" And 3))
    M1214 = R
    Exit Function
EH:
    M1214 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1215() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" = 0))
    M1215 = R
    Exit Function
EH:
    M1215 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1216() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" \ 2))
    M1216 = R
    Exit Function
EH:
    M1216 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1217() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" Mod 2))
    M1217 = R
    Exit Function
EH:
    M1217 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1218() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" ^ 2))
    M1218 = R
    Exit Function
EH:
    M1218 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1219() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(#1/1/2000#)))
    M1219 = R
    Exit Function
EH:
    M1219 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1220() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(#1/1/2000#)))
    M1220 = R
    Exit Function
EH:
    M1220 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1221() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(#1/1/2000#)))
    M1221 = R
    Exit Function
EH:
    M1221 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1222() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(#1/1/2000#)))
    M1222 = R
    Exit Function
EH:
    M1222 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1223() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(#1/1/2000#)))
    M1223 = R
    Exit Function
EH:
    M1223 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1224() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# + 1))
    M1224 = R
    Exit Function
EH:
    M1224 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1225() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# & "a"))
    M1225 = R
    Exit Function
EH:
    M1225 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1226() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-#1/1/2000#))
    M1226 = R
    Exit Function
EH:
    M1226 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1227() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not #1/1/2000#))
    M1227 = R
    Exit Function
EH:
    M1227 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1228() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# And 3))
    M1228 = R
    Exit Function
EH:
    M1228 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1229() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# = 0))
    M1229 = R
    Exit Function
EH:
    M1229 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1230() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# \ 2))
    M1230 = R
    Exit Function
EH:
    M1230 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1231() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# Mod 2))
    M1231 = R
    Exit Function
EH:
    M1231 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1232() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# ^ 2))
    M1232 = R
    Exit Function
EH:
    M1232 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1233() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(#12:00:00 PM#)))
    M1233 = R
    Exit Function
EH:
    M1233 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1234() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(#12:00:00 PM#)))
    M1234 = R
    Exit Function
EH:
    M1234 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1235() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(#12:00:00 PM#)))
    M1235 = R
    Exit Function
EH:
    M1235 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1236() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(#12:00:00 PM#)))
    M1236 = R
    Exit Function
EH:
    M1236 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1237() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(#12:00:00 PM#)))
    M1237 = R
    Exit Function
EH:
    M1237 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1238() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# + 1))
    M1238 = R
    Exit Function
EH:
    M1238 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1239() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# & "a"))
    M1239 = R
    Exit Function
EH:
    M1239 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1240() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-#12:00:00 PM#))
    M1240 = R
    Exit Function
EH:
    M1240 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1241() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not #12:00:00 PM#))
    M1241 = R
    Exit Function
EH:
    M1241 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1242() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# And 3))
    M1242 = R
    Exit Function
EH:
    M1242 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1243() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# = 0))
    M1243 = R
    Exit Function
EH:
    M1243 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1244() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# \ 2))
    M1244 = R
    Exit Function
EH:
    M1244 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1245() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# Mod 2))
    M1245 = R
    Exit Function
EH:
    M1245 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1246() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#12:00:00 PM# ^ 2))
    M1246 = R
    Exit Function
EH:
    M1246 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1247() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(CCur(5))))
    M1247 = R
    Exit Function
EH:
    M1247 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1248() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(CCur(5))))
    M1248 = R
    Exit Function
EH:
    M1248 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1249() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(CCur(5))))
    M1249 = R
    Exit Function
EH:
    M1249 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1250() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(CCur(5))))
    M1250 = R
    Exit Function
EH:
    M1250 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1251() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(CCur(5))))
    M1251 = R
    Exit Function
EH:
    M1251 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1252() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) + 1))
    M1252 = R
    Exit Function
EH:
    M1252 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1253() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) & "a"))
    M1253 = R
    Exit Function
EH:
    M1253 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1254() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-CCur(5)))
    M1254 = R
    Exit Function
EH:
    M1254 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1255() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not CCur(5)))
    M1255 = R
    Exit Function
EH:
    M1255 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1256() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) And 3))
    M1256 = R
    Exit Function
EH:
    M1256 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1257() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) = 0))
    M1257 = R
    Exit Function
EH:
    M1257 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1258() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) \ 2))
    M1258 = R
    Exit Function
EH:
    M1258 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1259() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) Mod 2))
    M1259 = R
    Exit Function
EH:
    M1259 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1260() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(5) ^ 2))
    M1260 = R
    Exit Function
EH:
    M1260 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1261() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(CDec(5))))
    M1261 = R
    Exit Function
EH:
    M1261 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1262() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(CDec(5))))
    M1262 = R
    Exit Function
EH:
    M1262 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1263() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(CDec(5))))
    M1263 = R
    Exit Function
EH:
    M1263 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1264() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(CDec(5))))
    M1264 = R
    Exit Function
EH:
    M1264 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1265() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(CDec(5))))
    M1265 = R
    Exit Function
EH:
    M1265 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1266() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) + 1))
    M1266 = R
    Exit Function
EH:
    M1266 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1267() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) & "a"))
    M1267 = R
    Exit Function
EH:
    M1267 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1268() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-CDec(5)))
    M1268 = R
    Exit Function
EH:
    M1268 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1269() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not CDec(5)))
    M1269 = R
    Exit Function
EH:
    M1269 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1270() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) And 3))
    M1270 = R
    Exit Function
EH:
    M1270 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1271() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) = 0))
    M1271 = R
    Exit Function
EH:
    M1271 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1272() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) \ 2))
    M1272 = R
    Exit Function
EH:
    M1272 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1273() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) Mod 2))
    M1273 = R
    Exit Function
EH:
    M1273 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1274() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) ^ 2))
    M1274 = R
    Exit Function
EH:
    M1274 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1275() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(CByte(5))))
    M1275 = R
    Exit Function
EH:
    M1275 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1276() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(CByte(5))))
    M1276 = R
    Exit Function
EH:
    M1276 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1277() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(CByte(5))))
    M1277 = R
    Exit Function
EH:
    M1277 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1278() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(CByte(5))))
    M1278 = R
    Exit Function
EH:
    M1278 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1279() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(CByte(5))))
    M1279 = R
    Exit Function
EH:
    M1279 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1280() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) + 1))
    M1280 = R
    Exit Function
EH:
    M1280 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1281() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) & "a"))
    M1281 = R
    Exit Function
EH:
    M1281 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1282() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-CByte(5)))
    M1282 = R
    Exit Function
EH:
    M1282 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1283() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not CByte(5)))
    M1283 = R
    Exit Function
EH:
    M1283 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1284() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) And 3))
    M1284 = R
    Exit Function
EH:
    M1284 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1285() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) = 0))
    M1285 = R
    Exit Function
EH:
    M1285 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1286() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) \ 2))
    M1286 = R
    Exit Function
EH:
    M1286 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1287() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) Mod 2))
    M1287 = R
    Exit Function
EH:
    M1287 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1288() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(5) ^ 2))
    M1288 = R
    Exit Function
EH:
    M1288 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1289() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Str(CVErr(5))))
    M1289 = R
    Exit Function
EH:
    M1289 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1290() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(CVErr(5))))
    M1290 = R
    Exit Function
EH:
    M1290 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1291() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(CVErr(5))))
    M1291 = R
    Exit Function
EH:
    M1291 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1292() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(CVErr(5))))
    M1292 = R
    Exit Function
EH:
    M1292 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1293() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(CVErr(5))))
    M1293 = R
    Exit Function
EH:
    M1293 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1294() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) + 1))
    M1294 = R
    Exit Function
EH:
    M1294 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1295() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) & "a"))
    M1295 = R
    Exit Function
EH:
    M1295 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1296() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-CVErr(5)))
    M1296 = R
    Exit Function
EH:
    M1296 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1297() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not CVErr(5)))
    M1297 = R
    Exit Function
EH:
    M1297 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1298() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) And 3))
    M1298 = R
    Exit Function
EH:
    M1298 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1299() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) = 0))
    M1299 = R
    Exit Function
EH:
    M1299 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1300() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) \ 2))
    M1300 = R
    Exit Function
EH:
    M1300 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1301() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) Mod 2))
    M1301 = R
    Exit Function
EH:
    M1301 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1302() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(5) ^ 2))
    M1302 = R
    Exit Function
EH:
    M1302 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1303() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1, #1/31/2000#)))
    M1303 = R
    Exit Function
EH:
    M1303 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1304() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -1, #1/31/2000#)))
    M1304 = R
    Exit Function
EH:
    M1304 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1305() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 13, #1/31/2000#)))
    M1305 = R
    Exit Function
EH:
    M1305 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1306() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -13, #1/31/2000#)))
    M1306 = R
    Exit Function
EH:
    M1306 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1307() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 0, #1/31/2000#)))
    M1307 = R
    Exit Function
EH:
    M1307 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1308() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 2.5, #1/31/2000#)))
    M1308 = R
    Exit Function
EH:
    M1308 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1309() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -2.5, #1/31/2000#)))
    M1309 = R
    Exit Function
EH:
    M1309 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1310() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 0.5, #1/31/2000#)))
    M1310 = R
    Exit Function
EH:
    M1310 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1311() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1.5, #1/31/2000#)))
    M1311 = R
    Exit Function
EH:
    M1311 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1312() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1, #2/29/2000#)))
    M1312 = R
    Exit Function
EH:
    M1312 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1313() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -1, #2/29/2000#)))
    M1313 = R
    Exit Function
EH:
    M1313 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1314() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 13, #2/29/2000#)))
    M1314 = R
    Exit Function
EH:
    M1314 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1315() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -13, #2/29/2000#)))
    M1315 = R
    Exit Function
EH:
    M1315 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1316() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 0, #2/29/2000#)))
    M1316 = R
    Exit Function
EH:
    M1316 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1317() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 2.5, #2/29/2000#)))
    M1317 = R
    Exit Function
EH:
    M1317 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1318() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -2.5, #2/29/2000#)))
    M1318 = R
    Exit Function
EH:
    M1318 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1319() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 0.5, #2/29/2000#)))
    M1319 = R
    Exit Function
EH:
    M1319 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1320() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1.5, #2/29/2000#)))
    M1320 = R
    Exit Function
EH:
    M1320 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1321() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1, #12/31/1999 11:59:59 PM#)))
    M1321 = R
    Exit Function
EH:
    M1321 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1322() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -1, #12/31/1999 11:59:59 PM#)))
    M1322 = R
    Exit Function
EH:
    M1322 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1323() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 13, #12/31/1999 11:59:59 PM#)))
    M1323 = R
    Exit Function
EH:
    M1323 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1324() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -13, #12/31/1999 11:59:59 PM#)))
    M1324 = R
    Exit Function
EH:
    M1324 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1325() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 0, #12/31/1999 11:59:59 PM#)))
    M1325 = R
    Exit Function
EH:
    M1325 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1326() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 2.5, #12/31/1999 11:59:59 PM#)))
    M1326 = R
    Exit Function
EH:
    M1326 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1327() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -2.5, #12/31/1999 11:59:59 PM#)))
    M1327 = R
    Exit Function
EH:
    M1327 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1328() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 0.5, #12/31/1999 11:59:59 PM#)))
    M1328 = R
    Exit Function
EH:
    M1328 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1329() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1.5, #12/31/1999 11:59:59 PM#)))
    M1329 = R
    Exit Function
EH:
    M1329 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1330() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 1, #1/31/2000#)))
    M1330 = R
    Exit Function
EH:
    M1330 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1331() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -1, #1/31/2000#)))
    M1331 = R
    Exit Function
EH:
    M1331 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1332() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 13, #1/31/2000#)))
    M1332 = R
    Exit Function
EH:
    M1332 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1333() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -13, #1/31/2000#)))
    M1333 = R
    Exit Function
EH:
    M1333 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1334() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 0, #1/31/2000#)))
    M1334 = R
    Exit Function
EH:
    M1334 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1335() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 2.5, #1/31/2000#)))
    M1335 = R
    Exit Function
EH:
    M1335 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1336() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -2.5, #1/31/2000#)))
    M1336 = R
    Exit Function
EH:
    M1336 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1337() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 0.5, #1/31/2000#)))
    M1337 = R
    Exit Function
EH:
    M1337 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1338() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 1.5, #1/31/2000#)))
    M1338 = R
    Exit Function
EH:
    M1338 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1339() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 1, #2/29/2000#)))
    M1339 = R
    Exit Function
EH:
    M1339 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1340() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -1, #2/29/2000#)))
    M1340 = R
    Exit Function
EH:
    M1340 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1341() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 13, #2/29/2000#)))
    M1341 = R
    Exit Function
EH:
    M1341 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1342() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -13, #2/29/2000#)))
    M1342 = R
    Exit Function
EH:
    M1342 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1343() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 0, #2/29/2000#)))
    M1343 = R
    Exit Function
EH:
    M1343 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1344() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 2.5, #2/29/2000#)))
    M1344 = R
    Exit Function
EH:
    M1344 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1345() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -2.5, #2/29/2000#)))
    M1345 = R
    Exit Function
EH:
    M1345 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1346() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 0.5, #2/29/2000#)))
    M1346 = R
    Exit Function
EH:
    M1346 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1347() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 1.5, #2/29/2000#)))
    M1347 = R
    Exit Function
EH:
    M1347 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1348() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 1, #12/31/1999 11:59:59 PM#)))
    M1348 = R
    Exit Function
EH:
    M1348 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1349() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -1, #12/31/1999 11:59:59 PM#)))
    M1349 = R
    Exit Function
EH:
    M1349 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1350() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 13, #12/31/1999 11:59:59 PM#)))
    M1350 = R
    Exit Function
EH:
    M1350 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1351() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -13, #12/31/1999 11:59:59 PM#)))
    M1351 = R
    Exit Function
EH:
    M1351 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1352() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 0, #12/31/1999 11:59:59 PM#)))
    M1352 = R
    Exit Function
EH:
    M1352 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1353() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 2.5, #12/31/1999 11:59:59 PM#)))
    M1353 = R
    Exit Function
EH:
    M1353 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1354() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", -2.5, #12/31/1999 11:59:59 PM#)))
    M1354 = R
    Exit Function
EH:
    M1354 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1355() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 0.5, #12/31/1999 11:59:59 PM#)))
    M1355 = R
    Exit Function
EH:
    M1355 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1356() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("q", 1.5, #12/31/1999 11:59:59 PM#)))
    M1356 = R
    Exit Function
EH:
    M1356 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1357() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1, #1/31/2000#)))
    M1357 = R
    Exit Function
EH:
    M1357 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1358() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -1, #1/31/2000#)))
    M1358 = R
    Exit Function
EH:
    M1358 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1359() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 13, #1/31/2000#)))
    M1359 = R
    Exit Function
EH:
    M1359 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1360() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -13, #1/31/2000#)))
    M1360 = R
    Exit Function
EH:
    M1360 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1361() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 0, #1/31/2000#)))
    M1361 = R
    Exit Function
EH:
    M1361 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1362() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 2.5, #1/31/2000#)))
    M1362 = R
    Exit Function
EH:
    M1362 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1363() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -2.5, #1/31/2000#)))
    M1363 = R
    Exit Function
EH:
    M1363 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1364() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 0.5, #1/31/2000#)))
    M1364 = R
    Exit Function
EH:
    M1364 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1365() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1.5, #1/31/2000#)))
    M1365 = R
    Exit Function
EH:
    M1365 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1366() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1, #2/29/2000#)))
    M1366 = R
    Exit Function
EH:
    M1366 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1367() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -1, #2/29/2000#)))
    M1367 = R
    Exit Function
EH:
    M1367 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1368() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 13, #2/29/2000#)))
    M1368 = R
    Exit Function
EH:
    M1368 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1369() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -13, #2/29/2000#)))
    M1369 = R
    Exit Function
EH:
    M1369 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1370() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 0, #2/29/2000#)))
    M1370 = R
    Exit Function
EH:
    M1370 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1371() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 2.5, #2/29/2000#)))
    M1371 = R
    Exit Function
EH:
    M1371 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1372() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -2.5, #2/29/2000#)))
    M1372 = R
    Exit Function
EH:
    M1372 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1373() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 0.5, #2/29/2000#)))
    M1373 = R
    Exit Function
EH:
    M1373 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1374() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1.5, #2/29/2000#)))
    M1374 = R
    Exit Function
EH:
    M1374 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1375() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1, #12/31/1999 11:59:59 PM#)))
    M1375 = R
    Exit Function
EH:
    M1375 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1376() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -1, #12/31/1999 11:59:59 PM#)))
    M1376 = R
    Exit Function
EH:
    M1376 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1377() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 13, #12/31/1999 11:59:59 PM#)))
    M1377 = R
    Exit Function
EH:
    M1377 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1378() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -13, #12/31/1999 11:59:59 PM#)))
    M1378 = R
    Exit Function
EH:
    M1378 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1379() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 0, #12/31/1999 11:59:59 PM#)))
    M1379 = R
    Exit Function
EH:
    M1379 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1380() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 2.5, #12/31/1999 11:59:59 PM#)))
    M1380 = R
    Exit Function
EH:
    M1380 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1381() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -2.5, #12/31/1999 11:59:59 PM#)))
    M1381 = R
    Exit Function
EH:
    M1381 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1382() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 0.5, #12/31/1999 11:59:59 PM#)))
    M1382 = R
    Exit Function
EH:
    M1382 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1383() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1.5, #12/31/1999 11:59:59 PM#)))
    M1383 = R
    Exit Function
EH:
    M1383 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1384() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 1, #1/31/2000#)))
    M1384 = R
    Exit Function
EH:
    M1384 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1385() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -1, #1/31/2000#)))
    M1385 = R
    Exit Function
EH:
    M1385 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1386() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 13, #1/31/2000#)))
    M1386 = R
    Exit Function
EH:
    M1386 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1387() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -13, #1/31/2000#)))
    M1387 = R
    Exit Function
EH:
    M1387 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1388() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 0, #1/31/2000#)))
    M1388 = R
    Exit Function
EH:
    M1388 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1389() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 2.5, #1/31/2000#)))
    M1389 = R
    Exit Function
EH:
    M1389 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1390() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -2.5, #1/31/2000#)))
    M1390 = R
    Exit Function
EH:
    M1390 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1391() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 0.5, #1/31/2000#)))
    M1391 = R
    Exit Function
EH:
    M1391 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1392() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 1.5, #1/31/2000#)))
    M1392 = R
    Exit Function
EH:
    M1392 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1393() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 1, #2/29/2000#)))
    M1393 = R
    Exit Function
EH:
    M1393 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1394() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -1, #2/29/2000#)))
    M1394 = R
    Exit Function
EH:
    M1394 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1395() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 13, #2/29/2000#)))
    M1395 = R
    Exit Function
EH:
    M1395 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1396() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -13, #2/29/2000#)))
    M1396 = R
    Exit Function
EH:
    M1396 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1397() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 0, #2/29/2000#)))
    M1397 = R
    Exit Function
EH:
    M1397 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1398() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 2.5, #2/29/2000#)))
    M1398 = R
    Exit Function
EH:
    M1398 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1399() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -2.5, #2/29/2000#)))
    M1399 = R
    Exit Function
EH:
    M1399 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1400() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 0.5, #2/29/2000#)))
    M1400 = R
    Exit Function
EH:
    M1400 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1401() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 1.5, #2/29/2000#)))
    M1401 = R
    Exit Function
EH:
    M1401 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1402() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 1, #12/31/1999 11:59:59 PM#)))
    M1402 = R
    Exit Function
EH:
    M1402 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1403() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -1, #12/31/1999 11:59:59 PM#)))
    M1403 = R
    Exit Function
EH:
    M1403 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1404() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 13, #12/31/1999 11:59:59 PM#)))
    M1404 = R
    Exit Function
EH:
    M1404 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1405() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -13, #12/31/1999 11:59:59 PM#)))
    M1405 = R
    Exit Function
EH:
    M1405 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1406() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 0, #12/31/1999 11:59:59 PM#)))
    M1406 = R
    Exit Function
EH:
    M1406 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1407() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 2.5, #12/31/1999 11:59:59 PM#)))
    M1407 = R
    Exit Function
EH:
    M1407 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1408() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", -2.5, #12/31/1999 11:59:59 PM#)))
    M1408 = R
    Exit Function
EH:
    M1408 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1409() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 0.5, #12/31/1999 11:59:59 PM#)))
    M1409 = R
    Exit Function
EH:
    M1409 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1410() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("y", 1.5, #12/31/1999 11:59:59 PM#)))
    M1410 = R
    Exit Function
EH:
    M1410 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1411() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, #1/31/2000#)))
    M1411 = R
    Exit Function
EH:
    M1411 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1412() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -1, #1/31/2000#)))
    M1412 = R
    Exit Function
EH:
    M1412 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1413() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 13, #1/31/2000#)))
    M1413 = R
    Exit Function
EH:
    M1413 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1414() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -13, #1/31/2000#)))
    M1414 = R
    Exit Function
EH:
    M1414 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1415() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 0, #1/31/2000#)))
    M1415 = R
    Exit Function
EH:
    M1415 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1416() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 2.5, #1/31/2000#)))
    M1416 = R
    Exit Function
EH:
    M1416 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1417() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -2.5, #1/31/2000#)))
    M1417 = R
    Exit Function
EH:
    M1417 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1418() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 0.5, #1/31/2000#)))
    M1418 = R
    Exit Function
EH:
    M1418 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1419() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1.5, #1/31/2000#)))
    M1419 = R
    Exit Function
EH:
    M1419 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1420() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, #2/29/2000#)))
    M1420 = R
    Exit Function
EH:
    M1420 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1421() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -1, #2/29/2000#)))
    M1421 = R
    Exit Function
EH:
    M1421 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1422() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 13, #2/29/2000#)))
    M1422 = R
    Exit Function
EH:
    M1422 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1423() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -13, #2/29/2000#)))
    M1423 = R
    Exit Function
EH:
    M1423 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1424() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 0, #2/29/2000#)))
    M1424 = R
    Exit Function
EH:
    M1424 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1425() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 2.5, #2/29/2000#)))
    M1425 = R
    Exit Function
EH:
    M1425 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1426() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -2.5, #2/29/2000#)))
    M1426 = R
    Exit Function
EH:
    M1426 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1427() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 0.5, #2/29/2000#)))
    M1427 = R
    Exit Function
EH:
    M1427 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1428() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1.5, #2/29/2000#)))
    M1428 = R
    Exit Function
EH:
    M1428 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1429() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, #12/31/1999 11:59:59 PM#)))
    M1429 = R
    Exit Function
EH:
    M1429 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1430() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -1, #12/31/1999 11:59:59 PM#)))
    M1430 = R
    Exit Function
EH:
    M1430 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1431() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 13, #12/31/1999 11:59:59 PM#)))
    M1431 = R
    Exit Function
EH:
    M1431 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1432() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -13, #12/31/1999 11:59:59 PM#)))
    M1432 = R
    Exit Function
EH:
    M1432 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1433() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 0, #12/31/1999 11:59:59 PM#)))
    M1433 = R
    Exit Function
EH:
    M1433 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1434() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 2.5, #12/31/1999 11:59:59 PM#)))
    M1434 = R
    Exit Function
EH:
    M1434 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1435() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -2.5, #12/31/1999 11:59:59 PM#)))
    M1435 = R
    Exit Function
EH:
    M1435 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1436() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 0.5, #12/31/1999 11:59:59 PM#)))
    M1436 = R
    Exit Function
EH:
    M1436 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1437() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1.5, #12/31/1999 11:59:59 PM#)))
    M1437 = R
    Exit Function
EH:
    M1437 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1438() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 1, #1/31/2000#)))
    M1438 = R
    Exit Function
EH:
    M1438 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1439() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -1, #1/31/2000#)))
    M1439 = R
    Exit Function
EH:
    M1439 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1440() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 13, #1/31/2000#)))
    M1440 = R
    Exit Function
EH:
    M1440 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1441() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -13, #1/31/2000#)))
    M1441 = R
    Exit Function
EH:
    M1441 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1442() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 0, #1/31/2000#)))
    M1442 = R
    Exit Function
EH:
    M1442 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1443() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 2.5, #1/31/2000#)))
    M1443 = R
    Exit Function
EH:
    M1443 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1444() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -2.5, #1/31/2000#)))
    M1444 = R
    Exit Function
EH:
    M1444 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1445() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 0.5, #1/31/2000#)))
    M1445 = R
    Exit Function
EH:
    M1445 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1446() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 1.5, #1/31/2000#)))
    M1446 = R
    Exit Function
EH:
    M1446 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1447() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 1, #2/29/2000#)))
    M1447 = R
    Exit Function
EH:
    M1447 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1448() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -1, #2/29/2000#)))
    M1448 = R
    Exit Function
EH:
    M1448 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1449() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 13, #2/29/2000#)))
    M1449 = R
    Exit Function
EH:
    M1449 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1450() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -13, #2/29/2000#)))
    M1450 = R
    Exit Function
EH:
    M1450 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1451() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 0, #2/29/2000#)))
    M1451 = R
    Exit Function
EH:
    M1451 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1452() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 2.5, #2/29/2000#)))
    M1452 = R
    Exit Function
EH:
    M1452 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1453() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -2.5, #2/29/2000#)))
    M1453 = R
    Exit Function
EH:
    M1453 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1454() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 0.5, #2/29/2000#)))
    M1454 = R
    Exit Function
EH:
    M1454 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1455() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 1.5, #2/29/2000#)))
    M1455 = R
    Exit Function
EH:
    M1455 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1456() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 1, #12/31/1999 11:59:59 PM#)))
    M1456 = R
    Exit Function
EH:
    M1456 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1457() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -1, #12/31/1999 11:59:59 PM#)))
    M1457 = R
    Exit Function
EH:
    M1457 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1458() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 13, #12/31/1999 11:59:59 PM#)))
    M1458 = R
    Exit Function
EH:
    M1458 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1459() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -13, #12/31/1999 11:59:59 PM#)))
    M1459 = R
    Exit Function
EH:
    M1459 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1460() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 0, #12/31/1999 11:59:59 PM#)))
    M1460 = R
    Exit Function
EH:
    M1460 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1461() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 2.5, #12/31/1999 11:59:59 PM#)))
    M1461 = R
    Exit Function
EH:
    M1461 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1462() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", -2.5, #12/31/1999 11:59:59 PM#)))
    M1462 = R
    Exit Function
EH:
    M1462 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1463() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 0.5, #12/31/1999 11:59:59 PM#)))
    M1463 = R
    Exit Function
EH:
    M1463 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1464() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("w", 1.5, #12/31/1999 11:59:59 PM#)))
    M1464 = R
    Exit Function
EH:
    M1464 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1465() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 1, #1/31/2000#)))
    M1465 = R
    Exit Function
EH:
    M1465 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1466() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -1, #1/31/2000#)))
    M1466 = R
    Exit Function
EH:
    M1466 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1467() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 13, #1/31/2000#)))
    M1467 = R
    Exit Function
EH:
    M1467 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1468() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -13, #1/31/2000#)))
    M1468 = R
    Exit Function
EH:
    M1468 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1469() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 0, #1/31/2000#)))
    M1469 = R
    Exit Function
EH:
    M1469 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1470() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 2.5, #1/31/2000#)))
    M1470 = R
    Exit Function
EH:
    M1470 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1471() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -2.5, #1/31/2000#)))
    M1471 = R
    Exit Function
EH:
    M1471 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1472() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 0.5, #1/31/2000#)))
    M1472 = R
    Exit Function
EH:
    M1472 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1473() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 1.5, #1/31/2000#)))
    M1473 = R
    Exit Function
EH:
    M1473 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1474() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 1, #2/29/2000#)))
    M1474 = R
    Exit Function
EH:
    M1474 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1475() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -1, #2/29/2000#)))
    M1475 = R
    Exit Function
EH:
    M1475 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1476() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 13, #2/29/2000#)))
    M1476 = R
    Exit Function
EH:
    M1476 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1477() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -13, #2/29/2000#)))
    M1477 = R
    Exit Function
EH:
    M1477 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1478() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 0, #2/29/2000#)))
    M1478 = R
    Exit Function
EH:
    M1478 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1479() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 2.5, #2/29/2000#)))
    M1479 = R
    Exit Function
EH:
    M1479 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1480() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -2.5, #2/29/2000#)))
    M1480 = R
    Exit Function
EH:
    M1480 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1481() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 0.5, #2/29/2000#)))
    M1481 = R
    Exit Function
EH:
    M1481 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1482() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 1.5, #2/29/2000#)))
    M1482 = R
    Exit Function
EH:
    M1482 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1483() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 1, #12/31/1999 11:59:59 PM#)))
    M1483 = R
    Exit Function
EH:
    M1483 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1484() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -1, #12/31/1999 11:59:59 PM#)))
    M1484 = R
    Exit Function
EH:
    M1484 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1485() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 13, #12/31/1999 11:59:59 PM#)))
    M1485 = R
    Exit Function
EH:
    M1485 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1486() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -13, #12/31/1999 11:59:59 PM#)))
    M1486 = R
    Exit Function
EH:
    M1486 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1487() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 0, #12/31/1999 11:59:59 PM#)))
    M1487 = R
    Exit Function
EH:
    M1487 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1488() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 2.5, #12/31/1999 11:59:59 PM#)))
    M1488 = R
    Exit Function
EH:
    M1488 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1489() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", -2.5, #12/31/1999 11:59:59 PM#)))
    M1489 = R
    Exit Function
EH:
    M1489 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1490() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 0.5, #12/31/1999 11:59:59 PM#)))
    M1490 = R
    Exit Function
EH:
    M1490 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1491() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ww", 1.5, #12/31/1999 11:59:59 PM#)))
    M1491 = R
    Exit Function
EH:
    M1491 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1492() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 1, #1/31/2000#)))
    M1492 = R
    Exit Function
EH:
    M1492 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1493() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -1, #1/31/2000#)))
    M1493 = R
    Exit Function
EH:
    M1493 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1494() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 13, #1/31/2000#)))
    M1494 = R
    Exit Function
EH:
    M1494 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1495() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -13, #1/31/2000#)))
    M1495 = R
    Exit Function
EH:
    M1495 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1496() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0, #1/31/2000#)))
    M1496 = R
    Exit Function
EH:
    M1496 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1497() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 2.5, #1/31/2000#)))
    M1497 = R
    Exit Function
EH:
    M1497 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1498() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -2.5, #1/31/2000#)))
    M1498 = R
    Exit Function
EH:
    M1498 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1499() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0.5, #1/31/2000#)))
    M1499 = R
    Exit Function
EH:
    M1499 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1500() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 1.5, #1/31/2000#)))
    M1500 = R
    Exit Function
EH:
    M1500 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1501() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 1, #2/29/2000#)))
    M1501 = R
    Exit Function
EH:
    M1501 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1502() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -1, #2/29/2000#)))
    M1502 = R
    Exit Function
EH:
    M1502 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1503() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 13, #2/29/2000#)))
    M1503 = R
    Exit Function
EH:
    M1503 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1504() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -13, #2/29/2000#)))
    M1504 = R
    Exit Function
EH:
    M1504 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1505() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0, #2/29/2000#)))
    M1505 = R
    Exit Function
EH:
    M1505 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1506() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 2.5, #2/29/2000#)))
    M1506 = R
    Exit Function
EH:
    M1506 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1507() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -2.5, #2/29/2000#)))
    M1507 = R
    Exit Function
EH:
    M1507 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1508() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0.5, #2/29/2000#)))
    M1508 = R
    Exit Function
EH:
    M1508 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1509() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 1.5, #2/29/2000#)))
    M1509 = R
    Exit Function
EH:
    M1509 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1510() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 1, #12/31/1999 11:59:59 PM#)))
    M1510 = R
    Exit Function
EH:
    M1510 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1511() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -1, #12/31/1999 11:59:59 PM#)))
    M1511 = R
    Exit Function
EH:
    M1511 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1512() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 13, #12/31/1999 11:59:59 PM#)))
    M1512 = R
    Exit Function
EH:
    M1512 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1513() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -13, #12/31/1999 11:59:59 PM#)))
    M1513 = R
    Exit Function
EH:
    M1513 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1514() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0, #12/31/1999 11:59:59 PM#)))
    M1514 = R
    Exit Function
EH:
    M1514 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1515() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 2.5, #12/31/1999 11:59:59 PM#)))
    M1515 = R
    Exit Function
EH:
    M1515 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1516() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", -2.5, #12/31/1999 11:59:59 PM#)))
    M1516 = R
    Exit Function
EH:
    M1516 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1517() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0.5, #12/31/1999 11:59:59 PM#)))
    M1517 = R
    Exit Function
EH:
    M1517 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1518() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 1.5, #12/31/1999 11:59:59 PM#)))
    M1518 = R
    Exit Function
EH:
    M1518 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1519() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1, #1/31/2000#)))
    M1519 = R
    Exit Function
EH:
    M1519 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1520() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -1, #1/31/2000#)))
    M1520 = R
    Exit Function
EH:
    M1520 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1521() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 13, #1/31/2000#)))
    M1521 = R
    Exit Function
EH:
    M1521 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1522() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -13, #1/31/2000#)))
    M1522 = R
    Exit Function
EH:
    M1522 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1523() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 0, #1/31/2000#)))
    M1523 = R
    Exit Function
EH:
    M1523 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1524() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 2.5, #1/31/2000#)))
    M1524 = R
    Exit Function
EH:
    M1524 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1525() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -2.5, #1/31/2000#)))
    M1525 = R
    Exit Function
EH:
    M1525 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1526() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 0.5, #1/31/2000#)))
    M1526 = R
    Exit Function
EH:
    M1526 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1527() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1.5, #1/31/2000#)))
    M1527 = R
    Exit Function
EH:
    M1527 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1528() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1, #2/29/2000#)))
    M1528 = R
    Exit Function
EH:
    M1528 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1529() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -1, #2/29/2000#)))
    M1529 = R
    Exit Function
EH:
    M1529 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1530() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 13, #2/29/2000#)))
    M1530 = R
    Exit Function
EH:
    M1530 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1531() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -13, #2/29/2000#)))
    M1531 = R
    Exit Function
EH:
    M1531 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1532() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 0, #2/29/2000#)))
    M1532 = R
    Exit Function
EH:
    M1532 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1533() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 2.5, #2/29/2000#)))
    M1533 = R
    Exit Function
EH:
    M1533 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1534() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -2.5, #2/29/2000#)))
    M1534 = R
    Exit Function
EH:
    M1534 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1535() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 0.5, #2/29/2000#)))
    M1535 = R
    Exit Function
EH:
    M1535 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1536() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1.5, #2/29/2000#)))
    M1536 = R
    Exit Function
EH:
    M1536 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1537() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1, #12/31/1999 11:59:59 PM#)))
    M1537 = R
    Exit Function
EH:
    M1537 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1538() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -1, #12/31/1999 11:59:59 PM#)))
    M1538 = R
    Exit Function
EH:
    M1538 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1539() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 13, #12/31/1999 11:59:59 PM#)))
    M1539 = R
    Exit Function
EH:
    M1539 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1540() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -13, #12/31/1999 11:59:59 PM#)))
    M1540 = R
    Exit Function
EH:
    M1540 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1541() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 0, #12/31/1999 11:59:59 PM#)))
    M1541 = R
    Exit Function
EH:
    M1541 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1542() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 2.5, #12/31/1999 11:59:59 PM#)))
    M1542 = R
    Exit Function
EH:
    M1542 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1543() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", -2.5, #12/31/1999 11:59:59 PM#)))
    M1543 = R
    Exit Function
EH:
    M1543 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1544() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 0.5, #12/31/1999 11:59:59 PM#)))
    M1544 = R
    Exit Function
EH:
    M1544 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1545() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1.5, #12/31/1999 11:59:59 PM#)))
    M1545 = R
    Exit Function
EH:
    M1545 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1546() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1, #1/31/2000#)))
    M1546 = R
    Exit Function
EH:
    M1546 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1547() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -1, #1/31/2000#)))
    M1547 = R
    Exit Function
EH:
    M1547 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1548() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 13, #1/31/2000#)))
    M1548 = R
    Exit Function
EH:
    M1548 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1549() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -13, #1/31/2000#)))
    M1549 = R
    Exit Function
EH:
    M1549 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1550() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0, #1/31/2000#)))
    M1550 = R
    Exit Function
EH:
    M1550 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1551() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 2.5, #1/31/2000#)))
    M1551 = R
    Exit Function
EH:
    M1551 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1552() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -2.5, #1/31/2000#)))
    M1552 = R
    Exit Function
EH:
    M1552 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1553() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0.5, #1/31/2000#)))
    M1553 = R
    Exit Function
EH:
    M1553 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1554() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1.5, #1/31/2000#)))
    M1554 = R
    Exit Function
EH:
    M1554 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1555() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1, #2/29/2000#)))
    M1555 = R
    Exit Function
EH:
    M1555 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1556() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -1, #2/29/2000#)))
    M1556 = R
    Exit Function
EH:
    M1556 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1557() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 13, #2/29/2000#)))
    M1557 = R
    Exit Function
EH:
    M1557 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1558() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -13, #2/29/2000#)))
    M1558 = R
    Exit Function
EH:
    M1558 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1559() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0, #2/29/2000#)))
    M1559 = R
    Exit Function
EH:
    M1559 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1560() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 2.5, #2/29/2000#)))
    M1560 = R
    Exit Function
EH:
    M1560 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1561() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -2.5, #2/29/2000#)))
    M1561 = R
    Exit Function
EH:
    M1561 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1562() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0.5, #2/29/2000#)))
    M1562 = R
    Exit Function
EH:
    M1562 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1563() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1.5, #2/29/2000#)))
    M1563 = R
    Exit Function
EH:
    M1563 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1564() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1, #12/31/1999 11:59:59 PM#)))
    M1564 = R
    Exit Function
EH:
    M1564 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1565() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -1, #12/31/1999 11:59:59 PM#)))
    M1565 = R
    Exit Function
EH:
    M1565 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1566() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 13, #12/31/1999 11:59:59 PM#)))
    M1566 = R
    Exit Function
EH:
    M1566 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1567() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -13, #12/31/1999 11:59:59 PM#)))
    M1567 = R
    Exit Function
EH:
    M1567 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1568() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0, #12/31/1999 11:59:59 PM#)))
    M1568 = R
    Exit Function
EH:
    M1568 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1569() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 2.5, #12/31/1999 11:59:59 PM#)))
    M1569 = R
    Exit Function
EH:
    M1569 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1570() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", -2.5, #12/31/1999 11:59:59 PM#)))
    M1570 = R
    Exit Function
EH:
    M1570 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1571() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0.5, #12/31/1999 11:59:59 PM#)))
    M1571 = R
    Exit Function
EH:
    M1571 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1572() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1.5, #12/31/1999 11:59:59 PM#)))
    M1572 = R
    Exit Function
EH:
    M1572 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1573() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("x", 1, #1/1/2000#)))
    M1573 = R
    Exit Function
EH:
    M1573 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1574() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("", 1, #1/1/2000#)))
    M1574 = R
    Exit Function
EH:
    M1574 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1575() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yy", 1, #1/1/2000#)))
    M1575 = R
    Exit Function
EH:
    M1575 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1576() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("Y", 1, #1/1/2000#)))
    M1576 = R
    Exit Function
EH:
    M1576 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1577() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("YYYY", 1, #1/1/2000#)))
    M1577 = R
    Exit Function
EH:
    M1577 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1578() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("mm", 1, #1/1/2000#)))
    M1578 = R
    Exit Function
EH:
    M1578 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1579() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("dd", 1, #1/1/2000#)))
    M1579 = R
    Exit Function
EH:
    M1579 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1580() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("qq", 1, #1/1/2000#)))
    M1580 = R
    Exit Function
EH:
    M1580 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1581() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("hh", 1, #1/1/2000#)))
    M1581 = R
    Exit Function
EH:
    M1581 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1582() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("nn", 1, #1/1/2000#)))
    M1582 = R
    Exit Function
EH:
    M1582 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1583() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("ss", 1, #1/1/2000#)))
    M1583 = R
    Exit Function
EH:
    M1583 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1584() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd(Null, 1, #1/1/2000#)))
    M1584 = R
    Exit Function
EH:
    M1584 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1585() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd(5, 1, #1/1/2000#)))
    M1585 = R
    Exit Function
EH:
    M1585 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1586() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, Null)))
    M1586 = R
    Exit Function
EH:
    M1586 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1587() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", Null, #1/1/2000#)))
    M1587 = R
    Exit Function
EH:
    M1587 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1588() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, "1/1/2000")))
    M1588 = R
    Exit Function
EH:
    M1588 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1589() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, "abc")))
    M1589 = R
    Exit Function
EH:
    M1589 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1590() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", "1", #1/1/2000#)))
    M1590 = R
    Exit Function
EH:
    M1590 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1591() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 8000, #1/1/2000#)))
    M1591 = R
    Exit Function
EH:
    M1591 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1592() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 7999, #1/1/2000#)))
    M1592 = R
    Exit Function
EH:
    M1592 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1593() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -1900, #1/1/2000#)))
    M1593 = R
    Exit Function
EH:
    M1593 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1594() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", -1999, #1/1/2000#)))
    M1594 = R
    Exit Function
EH:
    M1594 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1595() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, #12/31/9999#)))
    M1595 = R
    Exit Function
EH:
    M1595 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1596() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 1, #12/31/9999 11:59:59 PM#)))
    M1596 = R
    Exit Function
EH:
    M1596 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1597() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", -1, #1/1/100#)))
    M1597 = R
    Exit Function
EH:
    M1597 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1598() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, 36526)))
    M1598 = R
    Exit Function
EH:
    M1598 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1599() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1, Empty)))
    M1599 = R
    Exit Function
EH:
    M1599 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1600() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1, #1/31/2001#)))
    M1600 = R
    Exit Function
EH:
    M1600 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1601() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", 1, #1/30/2001#)))
    M1601 = R
    Exit Function
EH:
    M1601 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1602() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("yyyy", 1, #2/29/2000#)))
    M1602 = R
    Exit Function
EH:
    M1602 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1603() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("m", -1, #3/31/2001#)))
    M1603 = R
    Exit Function
EH:
    M1603 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1604() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 25, #1/1/2000#)))
    M1604 = R
    Exit Function
EH:
    M1604 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1605() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("n", 1441, #1/1/2000#)))
    M1605 = R
    Exit Function
EH:
    M1605 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1606() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 86401, #1/1/2000#)))
    M1606 = R
    Exit Function
EH:
    M1606 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1607() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("d", 1.5, #1/1/2000#)))
    M1607 = R
    Exit Function
EH:
    M1607 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1608() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("h", 0.5, #1/1/2000#)))
    M1608 = R
    Exit Function
EH:
    M1608 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1609() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateAdd("s", 0.5, #1/1/2000#)))
    M1609 = R
    Exit Function
EH:
    M1609 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1610() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("yyyy", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("yyyy", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1610 = R
    Exit Function
EH:
    M1610 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1611() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("yyyy", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("yyyy", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1611 = R
    Exit Function
EH:
    M1611 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1612() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("yyyy", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("yyyy", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1612 = R
    Exit Function
EH:
    M1612 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1613() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("yyyy", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("yyyy", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1613 = R
    Exit Function
EH:
    M1613 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1614() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("yyyy", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("yyyy", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1614 = R
    Exit Function
EH:
    M1614 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1615() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("yyyy", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("yyyy", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1615 = R
    Exit Function
EH:
    M1615 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1616() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("yyyy", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("yyyy", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1616 = R
    Exit Function
EH:
    M1616 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1617() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("yyyy", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("yyyy", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1617 = R
    Exit Function
EH:
    M1617 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1618() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("yyyy", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("yyyy", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1618 = R
    Exit Function
EH:
    M1618 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1619() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("yyyy", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("yyyy", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1619 = R
    Exit Function
EH:
    M1619 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1620() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("yyyy", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("yyyy", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("yyyy", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("yyyy", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("yyyy", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1620 = R
    Exit Function
EH:
    M1620 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1621() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("q", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("q", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1621 = R
    Exit Function
EH:
    M1621 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1622() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("q", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("q", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1622 = R
    Exit Function
EH:
    M1622 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1623() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("q", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("q", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1623 = R
    Exit Function
EH:
    M1623 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1624() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("q", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("q", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1624 = R
    Exit Function
EH:
    M1624 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1625() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("q", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("q", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1625 = R
    Exit Function
EH:
    M1625 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1626() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("q", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("q", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1626 = R
    Exit Function
EH:
    M1626 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1627() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("q", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("q", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1627 = R
    Exit Function
EH:
    M1627 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1628() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("q", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("q", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1628 = R
    Exit Function
EH:
    M1628 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1629() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("q", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("q", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1629 = R
    Exit Function
EH:
    M1629 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1630() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("q", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("q", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1630 = R
    Exit Function
EH:
    M1630 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1631() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("q", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("q", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("q", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("q", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("q", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1631 = R
    Exit Function
EH:
    M1631 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1632() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("m", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("m", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1632 = R
    Exit Function
EH:
    M1632 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1633() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("m", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("m", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1633 = R
    Exit Function
EH:
    M1633 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1634() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("m", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("m", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1634 = R
    Exit Function
EH:
    M1634 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1635() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("m", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("m", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1635 = R
    Exit Function
EH:
    M1635 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1636() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("m", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("m", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1636 = R
    Exit Function
EH:
    M1636 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1637() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("m", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("m", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1637 = R
    Exit Function
EH:
    M1637 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1638() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("m", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("m", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1638 = R
    Exit Function
EH:
    M1638 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1639() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("m", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("m", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1639 = R
    Exit Function
EH:
    M1639 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1640() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("m", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("m", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1640 = R
    Exit Function
EH:
    M1640 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1641() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("m", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("m", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1641 = R
    Exit Function
EH:
    M1641 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1642() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("m", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("m", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("m", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("m", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("m", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1642 = R
    Exit Function
EH:
    M1642 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1643() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("y", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("y", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1643 = R
    Exit Function
EH:
    M1643 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1644() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("y", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("y", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1644 = R
    Exit Function
EH:
    M1644 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1645() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("y", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("y", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1645 = R
    Exit Function
EH:
    M1645 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1646() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("y", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("y", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1646 = R
    Exit Function
EH:
    M1646 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1647() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("y", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("y", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1647 = R
    Exit Function
EH:
    M1647 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1648() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("y", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("y", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1648 = R
    Exit Function
EH:
    M1648 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1649() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("y", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("y", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1649 = R
    Exit Function
EH:
    M1649 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1650() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("y", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("y", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1650 = R
    Exit Function
EH:
    M1650 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1651() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("y", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("y", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1651 = R
    Exit Function
EH:
    M1651 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1652() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("y", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("y", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1652 = R
    Exit Function
EH:
    M1652 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1653() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("y", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("y", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("y", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("y", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("y", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1653 = R
    Exit Function
EH:
    M1653 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1654() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("d", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("d", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1654 = R
    Exit Function
EH:
    M1654 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1655() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("d", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("d", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1655 = R
    Exit Function
EH:
    M1655 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1656() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("d", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("d", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1656 = R
    Exit Function
EH:
    M1656 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1657() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("d", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("d", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1657 = R
    Exit Function
EH:
    M1657 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1658() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("d", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("d", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1658 = R
    Exit Function
EH:
    M1658 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1659() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("d", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("d", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1659 = R
    Exit Function
EH:
    M1659 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1660() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("d", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("d", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1660 = R
    Exit Function
EH:
    M1660 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1661() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("d", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("d", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1661 = R
    Exit Function
EH:
    M1661 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1662() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("d", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("d", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1662 = R
    Exit Function
EH:
    M1662 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1663() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("d", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("d", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1663 = R
    Exit Function
EH:
    M1663 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1664() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("d", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("d", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("d", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("d", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("d", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1664 = R
    Exit Function
EH:
    M1664 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1665() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("w", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("w", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1665 = R
    Exit Function
EH:
    M1665 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1666() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("w", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("w", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1666 = R
    Exit Function
EH:
    M1666 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1667() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("w", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("w", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1667 = R
    Exit Function
EH:
    M1667 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1668() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("w", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("w", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1668 = R
    Exit Function
EH:
    M1668 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1669() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("w", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("w", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1669 = R
    Exit Function
EH:
    M1669 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1670() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("w", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("w", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1670 = R
    Exit Function
EH:
    M1670 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1671() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("w", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("w", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1671 = R
    Exit Function
EH:
    M1671 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1672() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("w", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("w", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1672 = R
    Exit Function
EH:
    M1672 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1673() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("w", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("w", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1673 = R
    Exit Function
EH:
    M1673 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1674() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("w", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("w", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1674 = R
    Exit Function
EH:
    M1674 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1675() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("w", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("w", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("w", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("w", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("w", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1675 = R
    Exit Function
EH:
    M1675 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1676() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("ww", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("ww", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1676 = R
    Exit Function
EH:
    M1676 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1677() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("ww", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("ww", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1677 = R
    Exit Function
EH:
    M1677 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1678() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("ww", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("ww", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1678 = R
    Exit Function
EH:
    M1678 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1679() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("ww", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("ww", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1679 = R
    Exit Function
EH:
    M1679 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1680() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("ww", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("ww", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1680 = R
    Exit Function
EH:
    M1680 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1681() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("ww", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("ww", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1681 = R
    Exit Function
EH:
    M1681 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1682() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("ww", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("ww", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1682 = R
    Exit Function
EH:
    M1682 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1683() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("ww", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("ww", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1683 = R
    Exit Function
EH:
    M1683 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1684() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("ww", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("ww", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1684 = R
    Exit Function
EH:
    M1684 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1685() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("ww", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("ww", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1685 = R
    Exit Function
EH:
    M1685 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1686() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("ww", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("ww", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("ww", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("ww", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("ww", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1686 = R
    Exit Function
EH:
    M1686 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1687() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("h", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("h", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1687 = R
    Exit Function
EH:
    M1687 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1688() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("h", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("h", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1688 = R
    Exit Function
EH:
    M1688 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1689() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("h", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("h", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1689 = R
    Exit Function
EH:
    M1689 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1690() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("h", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("h", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1690 = R
    Exit Function
EH:
    M1690 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1691() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("h", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("h", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1691 = R
    Exit Function
EH:
    M1691 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1692() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("h", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("h", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1692 = R
    Exit Function
EH:
    M1692 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1693() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("h", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("h", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1693 = R
    Exit Function
EH:
    M1693 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1694() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("h", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("h", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1694 = R
    Exit Function
EH:
    M1694 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1695() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("h", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("h", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1695 = R
    Exit Function
EH:
    M1695 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1696() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("h", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("h", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1696 = R
    Exit Function
EH:
    M1696 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1697() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("h", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("h", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("h", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("h", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("h", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1697 = R
    Exit Function
EH:
    M1697 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1698() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("n", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("n", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1698 = R
    Exit Function
EH:
    M1698 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1699() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("n", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("n", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1699 = R
    Exit Function
EH:
    M1699 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1700() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("n", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("n", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1700 = R
    Exit Function
EH:
    M1700 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1701() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("n", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("n", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1701 = R
    Exit Function
EH:
    M1701 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1702() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("n", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("n", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1702 = R
    Exit Function
EH:
    M1702 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1703() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("n", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("n", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1703 = R
    Exit Function
EH:
    M1703 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1704() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("n", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("n", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1704 = R
    Exit Function
EH:
    M1704 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1705() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("n", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("n", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1705 = R
    Exit Function
EH:
    M1705 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1706() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("n", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("n", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1706 = R
    Exit Function
EH:
    M1706 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1707() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("n", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("n", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1707 = R
    Exit Function
EH:
    M1707 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1708() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("n", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("n", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("n", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("n", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("n", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1708 = R
    Exit Function
EH:
    M1708 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1709() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #1/1/2000#, #1/1/2001#) & "|" & DateDiff("s", #1/1/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("s", #1/1/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #1/1/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #1/1/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1709 = R
    Exit Function
EH:
    M1709 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1710() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #12/31/1999#, #1/1/2000#) & "|" & DateDiff("s", #12/31/1999#, #1/1/2000#, vbMonday) & "|" & DateDiff("s", #12/31/1999#, #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #12/31/1999#, #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #12/31/1999#, #1/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1710 = R
    Exit Function
EH:
    M1710 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1711() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #1/1/2000#, #12/31/1999#) & "|" & DateDiff("s", #1/1/2000#, #12/31/1999#, vbMonday) & "|" & DateDiff("s", #1/1/2000#, #12/31/1999#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #1/1/2000#, #12/31/1999#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #1/1/2000#, #12/31/1999#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1711 = R
    Exit Function
EH:
    M1711 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1712() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #1/31/2000#, #3/1/2000#) & "|" & DateDiff("s", #1/31/2000#, #3/1/2000#, vbMonday) & "|" & DateDiff("s", #1/31/2000#, #3/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #1/31/2000#, #3/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #1/31/2000#, #3/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1712 = R
    Exit Function
EH:
    M1712 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1713() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #12/31/2004#, #1/1/2005#) & "|" & DateDiff("s", #12/31/2004#, #1/1/2005#, vbMonday) & "|" & DateDiff("s", #12/31/2004#, #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #12/31/2004#, #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #12/31/2004#, #1/1/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1713 = R
    Exit Function
EH:
    M1713 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1714() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #1/1/2005#, #1/2/2005#) & "|" & DateDiff("s", #1/1/2005#, #1/2/2005#, vbMonday) & "|" & DateDiff("s", #1/1/2005#, #1/2/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #1/1/2005#, #1/2/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #1/1/2005#, #1/2/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1714 = R
    Exit Function
EH:
    M1714 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1715() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #1/2/2005#, #1/3/2005#) & "|" & DateDiff("s", #1/2/2005#, #1/3/2005#, vbMonday) & "|" & DateDiff("s", #1/2/2005#, #1/3/2005#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #1/2/2005#, #1/3/2005#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #1/2/2005#, #1/3/2005#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1715 = R
    Exit Function
EH:
    M1715 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1716() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #12/31/2000#, #1/1/2001#) & "|" & DateDiff("s", #12/31/2000#, #1/1/2001#, vbMonday) & "|" & DateDiff("s", #12/31/2000#, #1/1/2001#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #12/31/2000#, #1/1/2001#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #12/31/2000#, #1/1/2001#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1716 = R
    Exit Function
EH:
    M1716 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1717() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#) & "|" & DateDiff("s", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbMonday) & "|" & DateDiff("s", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #1/1/2000 11:59:59 PM#, #1/2/2000 12:00:01 AM#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1717 = R
    Exit Function
EH:
    M1717 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1718() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #3/31/2000#, #4/1/2000#) & "|" & DateDiff("s", #3/31/2000#, #4/1/2000#, vbMonday) & "|" & DateDiff("s", #3/31/2000#, #4/1/2000#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #3/31/2000#, #4/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #3/31/2000#, #4/1/2000#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1718 = R
    Exit Function
EH:
    M1718 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1719() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DateDiff("s", #6/15/2000#, #6/15/2000 0:00:01#) & "|" & DateDiff("s", #6/15/2000#, #6/15/2000 0:00:01#, vbMonday) & "|" & DateDiff("s", #6/15/2000#, #6/15/2000 0:00:01#, vbSunday, vbFirstFourDays) & "|" & DateDiff("s", #6/15/2000#, #6/15/2000 0:00:01#, vbSaturday, vbFirstFullWeek) & "|" & DateDiff("s", #6/15/2000#, #6/15/2000 0:00:01#, vbUseSystemDayOfWeek, vbFirstJan1)
    M1719 = R
    Exit Function
EH:
    M1719 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1720() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #1/1/2000#) & "|" & DatePart("yyyy", #1/1/2000#, vbMonday) & "|" & DatePart("yyyy", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #1/1/2000#, vbThursday, vbFirstJan1)
    M1720 = R
    Exit Function
EH:
    M1720 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1721() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #12/31/2000#) & "|" & DatePart("yyyy", #12/31/2000#, vbMonday) & "|" & DatePart("yyyy", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #12/31/2000#, vbThursday, vbFirstJan1)
    M1721 = R
    Exit Function
EH:
    M1721 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1722() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #1/1/2005#) & "|" & DatePart("yyyy", #1/1/2005#, vbMonday) & "|" & DatePart("yyyy", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #1/1/2005#, vbThursday, vbFirstJan1)
    M1722 = R
    Exit Function
EH:
    M1722 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1723() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #1/1/2006#) & "|" & DatePart("yyyy", #1/1/2006#, vbMonday) & "|" & DatePart("yyyy", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #1/1/2006#, vbThursday, vbFirstJan1)
    M1723 = R
    Exit Function
EH:
    M1723 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1724() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #1/1/2007#) & "|" & DatePart("yyyy", #1/1/2007#, vbMonday) & "|" & DatePart("yyyy", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #1/1/2007#, vbThursday, vbFirstJan1)
    M1724 = R
    Exit Function
EH:
    M1724 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1725() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #12/28/2008#) & "|" & DatePart("yyyy", #12/28/2008#, vbMonday) & "|" & DatePart("yyyy", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #12/28/2008#, vbThursday, vbFirstJan1)
    M1725 = R
    Exit Function
EH:
    M1725 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1726() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #12/29/2008#) & "|" & DatePart("yyyy", #12/29/2008#, vbMonday) & "|" & DatePart("yyyy", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #12/29/2008#, vbThursday, vbFirstJan1)
    M1726 = R
    Exit Function
EH:
    M1726 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1727() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #1/3/2000#) & "|" & DatePart("yyyy", #1/3/2000#, vbMonday) & "|" & DatePart("yyyy", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #1/3/2000#, vbThursday, vbFirstJan1)
    M1727 = R
    Exit Function
EH:
    M1727 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1728() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #1/4/2000#) & "|" & DatePart("yyyy", #1/4/2000#, vbMonday) & "|" & DatePart("yyyy", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #1/4/2000#, vbThursday, vbFirstJan1)
    M1728 = R
    Exit Function
EH:
    M1728 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1729() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #7/4/2000 13:14:15#) & "|" & DatePart("yyyy", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("yyyy", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1729 = R
    Exit Function
EH:
    M1729 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1730() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("yyyy", #2/29/2000 0:00:00#) & "|" & DatePart("yyyy", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("yyyy", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("yyyy", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("yyyy", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1730 = R
    Exit Function
EH:
    M1730 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1731() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #1/1/2000#) & "|" & DatePart("q", #1/1/2000#, vbMonday) & "|" & DatePart("q", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #1/1/2000#, vbThursday, vbFirstJan1)
    M1731 = R
    Exit Function
EH:
    M1731 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1732() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #12/31/2000#) & "|" & DatePart("q", #12/31/2000#, vbMonday) & "|" & DatePart("q", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #12/31/2000#, vbThursday, vbFirstJan1)
    M1732 = R
    Exit Function
EH:
    M1732 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1733() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #1/1/2005#) & "|" & DatePart("q", #1/1/2005#, vbMonday) & "|" & DatePart("q", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #1/1/2005#, vbThursday, vbFirstJan1)
    M1733 = R
    Exit Function
EH:
    M1733 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1734() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #1/1/2006#) & "|" & DatePart("q", #1/1/2006#, vbMonday) & "|" & DatePart("q", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #1/1/2006#, vbThursday, vbFirstJan1)
    M1734 = R
    Exit Function
EH:
    M1734 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1735() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #1/1/2007#) & "|" & DatePart("q", #1/1/2007#, vbMonday) & "|" & DatePart("q", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #1/1/2007#, vbThursday, vbFirstJan1)
    M1735 = R
    Exit Function
EH:
    M1735 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1736() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #12/28/2008#) & "|" & DatePart("q", #12/28/2008#, vbMonday) & "|" & DatePart("q", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #12/28/2008#, vbThursday, vbFirstJan1)
    M1736 = R
    Exit Function
EH:
    M1736 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1737() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #12/29/2008#) & "|" & DatePart("q", #12/29/2008#, vbMonday) & "|" & DatePart("q", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #12/29/2008#, vbThursday, vbFirstJan1)
    M1737 = R
    Exit Function
EH:
    M1737 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1738() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #1/3/2000#) & "|" & DatePart("q", #1/3/2000#, vbMonday) & "|" & DatePart("q", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #1/3/2000#, vbThursday, vbFirstJan1)
    M1738 = R
    Exit Function
EH:
    M1738 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1739() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #1/4/2000#) & "|" & DatePart("q", #1/4/2000#, vbMonday) & "|" & DatePart("q", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #1/4/2000#, vbThursday, vbFirstJan1)
    M1739 = R
    Exit Function
EH:
    M1739 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1740() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #7/4/2000 13:14:15#) & "|" & DatePart("q", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("q", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1740 = R
    Exit Function
EH:
    M1740 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1741() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("q", #2/29/2000 0:00:00#) & "|" & DatePart("q", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("q", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("q", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("q", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1741 = R
    Exit Function
EH:
    M1741 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1742() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #1/1/2000#) & "|" & DatePart("m", #1/1/2000#, vbMonday) & "|" & DatePart("m", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #1/1/2000#, vbThursday, vbFirstJan1)
    M1742 = R
    Exit Function
EH:
    M1742 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1743() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #12/31/2000#) & "|" & DatePart("m", #12/31/2000#, vbMonday) & "|" & DatePart("m", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #12/31/2000#, vbThursday, vbFirstJan1)
    M1743 = R
    Exit Function
EH:
    M1743 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1744() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #1/1/2005#) & "|" & DatePart("m", #1/1/2005#, vbMonday) & "|" & DatePart("m", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #1/1/2005#, vbThursday, vbFirstJan1)
    M1744 = R
    Exit Function
EH:
    M1744 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1745() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #1/1/2006#) & "|" & DatePart("m", #1/1/2006#, vbMonday) & "|" & DatePart("m", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #1/1/2006#, vbThursday, vbFirstJan1)
    M1745 = R
    Exit Function
EH:
    M1745 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1746() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #1/1/2007#) & "|" & DatePart("m", #1/1/2007#, vbMonday) & "|" & DatePart("m", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #1/1/2007#, vbThursday, vbFirstJan1)
    M1746 = R
    Exit Function
EH:
    M1746 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1747() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #12/28/2008#) & "|" & DatePart("m", #12/28/2008#, vbMonday) & "|" & DatePart("m", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #12/28/2008#, vbThursday, vbFirstJan1)
    M1747 = R
    Exit Function
EH:
    M1747 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1748() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #12/29/2008#) & "|" & DatePart("m", #12/29/2008#, vbMonday) & "|" & DatePart("m", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #12/29/2008#, vbThursday, vbFirstJan1)
    M1748 = R
    Exit Function
EH:
    M1748 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1749() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #1/3/2000#) & "|" & DatePart("m", #1/3/2000#, vbMonday) & "|" & DatePart("m", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #1/3/2000#, vbThursday, vbFirstJan1)
    M1749 = R
    Exit Function
EH:
    M1749 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1750() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #1/4/2000#) & "|" & DatePart("m", #1/4/2000#, vbMonday) & "|" & DatePart("m", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #1/4/2000#, vbThursday, vbFirstJan1)
    M1750 = R
    Exit Function
EH:
    M1750 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1751() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #7/4/2000 13:14:15#) & "|" & DatePart("m", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("m", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1751 = R
    Exit Function
EH:
    M1751 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1752() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("m", #2/29/2000 0:00:00#) & "|" & DatePart("m", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("m", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("m", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("m", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1752 = R
    Exit Function
EH:
    M1752 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1753() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #1/1/2000#) & "|" & DatePart("y", #1/1/2000#, vbMonday) & "|" & DatePart("y", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #1/1/2000#, vbThursday, vbFirstJan1)
    M1753 = R
    Exit Function
EH:
    M1753 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1754() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #12/31/2000#) & "|" & DatePart("y", #12/31/2000#, vbMonday) & "|" & DatePart("y", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #12/31/2000#, vbThursday, vbFirstJan1)
    M1754 = R
    Exit Function
EH:
    M1754 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1755() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #1/1/2005#) & "|" & DatePart("y", #1/1/2005#, vbMonday) & "|" & DatePart("y", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #1/1/2005#, vbThursday, vbFirstJan1)
    M1755 = R
    Exit Function
EH:
    M1755 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1756() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #1/1/2006#) & "|" & DatePart("y", #1/1/2006#, vbMonday) & "|" & DatePart("y", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #1/1/2006#, vbThursday, vbFirstJan1)
    M1756 = R
    Exit Function
EH:
    M1756 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1757() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #1/1/2007#) & "|" & DatePart("y", #1/1/2007#, vbMonday) & "|" & DatePart("y", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #1/1/2007#, vbThursday, vbFirstJan1)
    M1757 = R
    Exit Function
EH:
    M1757 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1758() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #12/28/2008#) & "|" & DatePart("y", #12/28/2008#, vbMonday) & "|" & DatePart("y", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #12/28/2008#, vbThursday, vbFirstJan1)
    M1758 = R
    Exit Function
EH:
    M1758 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1759() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #12/29/2008#) & "|" & DatePart("y", #12/29/2008#, vbMonday) & "|" & DatePart("y", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #12/29/2008#, vbThursday, vbFirstJan1)
    M1759 = R
    Exit Function
EH:
    M1759 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1760() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #1/3/2000#) & "|" & DatePart("y", #1/3/2000#, vbMonday) & "|" & DatePart("y", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #1/3/2000#, vbThursday, vbFirstJan1)
    M1760 = R
    Exit Function
EH:
    M1760 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1761() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #1/4/2000#) & "|" & DatePart("y", #1/4/2000#, vbMonday) & "|" & DatePart("y", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #1/4/2000#, vbThursday, vbFirstJan1)
    M1761 = R
    Exit Function
EH:
    M1761 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1762() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #7/4/2000 13:14:15#) & "|" & DatePart("y", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("y", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1762 = R
    Exit Function
EH:
    M1762 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1763() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("y", #2/29/2000 0:00:00#) & "|" & DatePart("y", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("y", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("y", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("y", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1763 = R
    Exit Function
EH:
    M1763 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1764() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #1/1/2000#) & "|" & DatePart("d", #1/1/2000#, vbMonday) & "|" & DatePart("d", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #1/1/2000#, vbThursday, vbFirstJan1)
    M1764 = R
    Exit Function
EH:
    M1764 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1765() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #12/31/2000#) & "|" & DatePart("d", #12/31/2000#, vbMonday) & "|" & DatePart("d", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #12/31/2000#, vbThursday, vbFirstJan1)
    M1765 = R
    Exit Function
EH:
    M1765 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1766() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #1/1/2005#) & "|" & DatePart("d", #1/1/2005#, vbMonday) & "|" & DatePart("d", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #1/1/2005#, vbThursday, vbFirstJan1)
    M1766 = R
    Exit Function
EH:
    M1766 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1767() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #1/1/2006#) & "|" & DatePart("d", #1/1/2006#, vbMonday) & "|" & DatePart("d", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #1/1/2006#, vbThursday, vbFirstJan1)
    M1767 = R
    Exit Function
EH:
    M1767 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1768() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #1/1/2007#) & "|" & DatePart("d", #1/1/2007#, vbMonday) & "|" & DatePart("d", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #1/1/2007#, vbThursday, vbFirstJan1)
    M1768 = R
    Exit Function
EH:
    M1768 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1769() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #12/28/2008#) & "|" & DatePart("d", #12/28/2008#, vbMonday) & "|" & DatePart("d", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #12/28/2008#, vbThursday, vbFirstJan1)
    M1769 = R
    Exit Function
EH:
    M1769 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1770() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #12/29/2008#) & "|" & DatePart("d", #12/29/2008#, vbMonday) & "|" & DatePart("d", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #12/29/2008#, vbThursday, vbFirstJan1)
    M1770 = R
    Exit Function
EH:
    M1770 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1771() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #1/3/2000#) & "|" & DatePart("d", #1/3/2000#, vbMonday) & "|" & DatePart("d", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #1/3/2000#, vbThursday, vbFirstJan1)
    M1771 = R
    Exit Function
EH:
    M1771 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1772() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #1/4/2000#) & "|" & DatePart("d", #1/4/2000#, vbMonday) & "|" & DatePart("d", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #1/4/2000#, vbThursday, vbFirstJan1)
    M1772 = R
    Exit Function
EH:
    M1772 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1773() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #7/4/2000 13:14:15#) & "|" & DatePart("d", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("d", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1773 = R
    Exit Function
EH:
    M1773 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1774() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("d", #2/29/2000 0:00:00#) & "|" & DatePart("d", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("d", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("d", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("d", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1774 = R
    Exit Function
EH:
    M1774 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1775() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #1/1/2000#) & "|" & DatePart("w", #1/1/2000#, vbMonday) & "|" & DatePart("w", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #1/1/2000#, vbThursday, vbFirstJan1)
    M1775 = R
    Exit Function
EH:
    M1775 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1776() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #12/31/2000#) & "|" & DatePart("w", #12/31/2000#, vbMonday) & "|" & DatePart("w", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #12/31/2000#, vbThursday, vbFirstJan1)
    M1776 = R
    Exit Function
EH:
    M1776 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1777() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #1/1/2005#) & "|" & DatePart("w", #1/1/2005#, vbMonday) & "|" & DatePart("w", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #1/1/2005#, vbThursday, vbFirstJan1)
    M1777 = R
    Exit Function
EH:
    M1777 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1778() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #1/1/2006#) & "|" & DatePart("w", #1/1/2006#, vbMonday) & "|" & DatePart("w", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #1/1/2006#, vbThursday, vbFirstJan1)
    M1778 = R
    Exit Function
EH:
    M1778 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1779() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #1/1/2007#) & "|" & DatePart("w", #1/1/2007#, vbMonday) & "|" & DatePart("w", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #1/1/2007#, vbThursday, vbFirstJan1)
    M1779 = R
    Exit Function
EH:
    M1779 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1780() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #12/28/2008#) & "|" & DatePart("w", #12/28/2008#, vbMonday) & "|" & DatePart("w", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #12/28/2008#, vbThursday, vbFirstJan1)
    M1780 = R
    Exit Function
EH:
    M1780 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1781() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #12/29/2008#) & "|" & DatePart("w", #12/29/2008#, vbMonday) & "|" & DatePart("w", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #12/29/2008#, vbThursday, vbFirstJan1)
    M1781 = R
    Exit Function
EH:
    M1781 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1782() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #1/3/2000#) & "|" & DatePart("w", #1/3/2000#, vbMonday) & "|" & DatePart("w", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #1/3/2000#, vbThursday, vbFirstJan1)
    M1782 = R
    Exit Function
EH:
    M1782 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1783() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #1/4/2000#) & "|" & DatePart("w", #1/4/2000#, vbMonday) & "|" & DatePart("w", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #1/4/2000#, vbThursday, vbFirstJan1)
    M1783 = R
    Exit Function
EH:
    M1783 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1784() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #7/4/2000 13:14:15#) & "|" & DatePart("w", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("w", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1784 = R
    Exit Function
EH:
    M1784 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1785() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("w", #2/29/2000 0:00:00#) & "|" & DatePart("w", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("w", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("w", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("w", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1785 = R
    Exit Function
EH:
    M1785 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1786() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #1/1/2000#) & "|" & DatePart("ww", #1/1/2000#, vbMonday) & "|" & DatePart("ww", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #1/1/2000#, vbThursday, vbFirstJan1)
    M1786 = R
    Exit Function
EH:
    M1786 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1787() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #12/31/2000#) & "|" & DatePart("ww", #12/31/2000#, vbMonday) & "|" & DatePart("ww", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #12/31/2000#, vbThursday, vbFirstJan1)
    M1787 = R
    Exit Function
EH:
    M1787 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1788() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #1/1/2005#) & "|" & DatePart("ww", #1/1/2005#, vbMonday) & "|" & DatePart("ww", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #1/1/2005#, vbThursday, vbFirstJan1)
    M1788 = R
    Exit Function
EH:
    M1788 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1789() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #1/1/2006#) & "|" & DatePart("ww", #1/1/2006#, vbMonday) & "|" & DatePart("ww", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #1/1/2006#, vbThursday, vbFirstJan1)
    M1789 = R
    Exit Function
EH:
    M1789 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1790() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #1/1/2007#) & "|" & DatePart("ww", #1/1/2007#, vbMonday) & "|" & DatePart("ww", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #1/1/2007#, vbThursday, vbFirstJan1)
    M1790 = R
    Exit Function
EH:
    M1790 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1791() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #12/28/2008#) & "|" & DatePart("ww", #12/28/2008#, vbMonday) & "|" & DatePart("ww", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #12/28/2008#, vbThursday, vbFirstJan1)
    M1791 = R
    Exit Function
EH:
    M1791 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1792() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #12/29/2008#) & "|" & DatePart("ww", #12/29/2008#, vbMonday) & "|" & DatePart("ww", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #12/29/2008#, vbThursday, vbFirstJan1)
    M1792 = R
    Exit Function
EH:
    M1792 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1793() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #1/3/2000#) & "|" & DatePart("ww", #1/3/2000#, vbMonday) & "|" & DatePart("ww", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #1/3/2000#, vbThursday, vbFirstJan1)
    M1793 = R
    Exit Function
EH:
    M1793 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1794() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #1/4/2000#) & "|" & DatePart("ww", #1/4/2000#, vbMonday) & "|" & DatePart("ww", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #1/4/2000#, vbThursday, vbFirstJan1)
    M1794 = R
    Exit Function
EH:
    M1794 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1795() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #7/4/2000 13:14:15#) & "|" & DatePart("ww", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("ww", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1795 = R
    Exit Function
EH:
    M1795 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1796() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("ww", #2/29/2000 0:00:00#) & "|" & DatePart("ww", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("ww", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("ww", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("ww", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1796 = R
    Exit Function
EH:
    M1796 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1797() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #1/1/2000#) & "|" & DatePart("h", #1/1/2000#, vbMonday) & "|" & DatePart("h", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #1/1/2000#, vbThursday, vbFirstJan1)
    M1797 = R
    Exit Function
EH:
    M1797 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1798() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #12/31/2000#) & "|" & DatePart("h", #12/31/2000#, vbMonday) & "|" & DatePart("h", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #12/31/2000#, vbThursday, vbFirstJan1)
    M1798 = R
    Exit Function
EH:
    M1798 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1799() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #1/1/2005#) & "|" & DatePart("h", #1/1/2005#, vbMonday) & "|" & DatePart("h", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #1/1/2005#, vbThursday, vbFirstJan1)
    M1799 = R
    Exit Function
EH:
    M1799 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1800() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #1/1/2006#) & "|" & DatePart("h", #1/1/2006#, vbMonday) & "|" & DatePart("h", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #1/1/2006#, vbThursday, vbFirstJan1)
    M1800 = R
    Exit Function
EH:
    M1800 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1801() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #1/1/2007#) & "|" & DatePart("h", #1/1/2007#, vbMonday) & "|" & DatePart("h", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #1/1/2007#, vbThursday, vbFirstJan1)
    M1801 = R
    Exit Function
EH:
    M1801 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1802() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #12/28/2008#) & "|" & DatePart("h", #12/28/2008#, vbMonday) & "|" & DatePart("h", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #12/28/2008#, vbThursday, vbFirstJan1)
    M1802 = R
    Exit Function
EH:
    M1802 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1803() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #12/29/2008#) & "|" & DatePart("h", #12/29/2008#, vbMonday) & "|" & DatePart("h", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #12/29/2008#, vbThursday, vbFirstJan1)
    M1803 = R
    Exit Function
EH:
    M1803 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1804() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #1/3/2000#) & "|" & DatePart("h", #1/3/2000#, vbMonday) & "|" & DatePart("h", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #1/3/2000#, vbThursday, vbFirstJan1)
    M1804 = R
    Exit Function
EH:
    M1804 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1805() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #1/4/2000#) & "|" & DatePart("h", #1/4/2000#, vbMonday) & "|" & DatePart("h", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #1/4/2000#, vbThursday, vbFirstJan1)
    M1805 = R
    Exit Function
EH:
    M1805 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1806() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #7/4/2000 13:14:15#) & "|" & DatePart("h", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("h", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1806 = R
    Exit Function
EH:
    M1806 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1807() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("h", #2/29/2000 0:00:00#) & "|" & DatePart("h", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("h", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("h", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("h", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1807 = R
    Exit Function
EH:
    M1807 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1808() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #1/1/2000#) & "|" & DatePart("n", #1/1/2000#, vbMonday) & "|" & DatePart("n", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #1/1/2000#, vbThursday, vbFirstJan1)
    M1808 = R
    Exit Function
EH:
    M1808 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1809() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #12/31/2000#) & "|" & DatePart("n", #12/31/2000#, vbMonday) & "|" & DatePart("n", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #12/31/2000#, vbThursday, vbFirstJan1)
    M1809 = R
    Exit Function
EH:
    M1809 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1810() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #1/1/2005#) & "|" & DatePart("n", #1/1/2005#, vbMonday) & "|" & DatePart("n", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #1/1/2005#, vbThursday, vbFirstJan1)
    M1810 = R
    Exit Function
EH:
    M1810 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1811() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #1/1/2006#) & "|" & DatePart("n", #1/1/2006#, vbMonday) & "|" & DatePart("n", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #1/1/2006#, vbThursday, vbFirstJan1)
    M1811 = R
    Exit Function
EH:
    M1811 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1812() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #1/1/2007#) & "|" & DatePart("n", #1/1/2007#, vbMonday) & "|" & DatePart("n", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #1/1/2007#, vbThursday, vbFirstJan1)
    M1812 = R
    Exit Function
EH:
    M1812 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1813() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #12/28/2008#) & "|" & DatePart("n", #12/28/2008#, vbMonday) & "|" & DatePart("n", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #12/28/2008#, vbThursday, vbFirstJan1)
    M1813 = R
    Exit Function
EH:
    M1813 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1814() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #12/29/2008#) & "|" & DatePart("n", #12/29/2008#, vbMonday) & "|" & DatePart("n", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #12/29/2008#, vbThursday, vbFirstJan1)
    M1814 = R
    Exit Function
EH:
    M1814 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1815() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #1/3/2000#) & "|" & DatePart("n", #1/3/2000#, vbMonday) & "|" & DatePart("n", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #1/3/2000#, vbThursday, vbFirstJan1)
    M1815 = R
    Exit Function
EH:
    M1815 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1816() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #1/4/2000#) & "|" & DatePart("n", #1/4/2000#, vbMonday) & "|" & DatePart("n", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #1/4/2000#, vbThursday, vbFirstJan1)
    M1816 = R
    Exit Function
EH:
    M1816 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1817() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #7/4/2000 13:14:15#) & "|" & DatePart("n", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("n", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1817 = R
    Exit Function
EH:
    M1817 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1818() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("n", #2/29/2000 0:00:00#) & "|" & DatePart("n", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("n", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("n", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("n", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1818 = R
    Exit Function
EH:
    M1818 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1819() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #1/1/2000#) & "|" & DatePart("s", #1/1/2000#, vbMonday) & "|" & DatePart("s", #1/1/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #1/1/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #1/1/2000#, vbThursday, vbFirstJan1)
    M1819 = R
    Exit Function
EH:
    M1819 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1820() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #12/31/2000#) & "|" & DatePart("s", #12/31/2000#, vbMonday) & "|" & DatePart("s", #12/31/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #12/31/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #12/31/2000#, vbThursday, vbFirstJan1)
    M1820 = R
    Exit Function
EH:
    M1820 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1821() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #1/1/2005#) & "|" & DatePart("s", #1/1/2005#, vbMonday) & "|" & DatePart("s", #1/1/2005#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #1/1/2005#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #1/1/2005#, vbThursday, vbFirstJan1)
    M1821 = R
    Exit Function
EH:
    M1821 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1822() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #1/1/2006#) & "|" & DatePart("s", #1/1/2006#, vbMonday) & "|" & DatePart("s", #1/1/2006#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #1/1/2006#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #1/1/2006#, vbThursday, vbFirstJan1)
    M1822 = R
    Exit Function
EH:
    M1822 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1823() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #1/1/2007#) & "|" & DatePart("s", #1/1/2007#, vbMonday) & "|" & DatePart("s", #1/1/2007#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #1/1/2007#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #1/1/2007#, vbThursday, vbFirstJan1)
    M1823 = R
    Exit Function
EH:
    M1823 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1824() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #12/28/2008#) & "|" & DatePart("s", #12/28/2008#, vbMonday) & "|" & DatePart("s", #12/28/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #12/28/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #12/28/2008#, vbThursday, vbFirstJan1)
    M1824 = R
    Exit Function
EH:
    M1824 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1825() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #12/29/2008#) & "|" & DatePart("s", #12/29/2008#, vbMonday) & "|" & DatePart("s", #12/29/2008#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #12/29/2008#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #12/29/2008#, vbThursday, vbFirstJan1)
    M1825 = R
    Exit Function
EH:
    M1825 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1826() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #1/3/2000#) & "|" & DatePart("s", #1/3/2000#, vbMonday) & "|" & DatePart("s", #1/3/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #1/3/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #1/3/2000#, vbThursday, vbFirstJan1)
    M1826 = R
    Exit Function
EH:
    M1826 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1827() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #1/4/2000#) & "|" & DatePart("s", #1/4/2000#, vbMonday) & "|" & DatePart("s", #1/4/2000#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #1/4/2000#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #1/4/2000#, vbThursday, vbFirstJan1)
    M1827 = R
    Exit Function
EH:
    M1827 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1828() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #7/4/2000 13:14:15#) & "|" & DatePart("s", #7/4/2000 13:14:15#, vbMonday) & "|" & DatePart("s", #7/4/2000 13:14:15#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #7/4/2000 13:14:15#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #7/4/2000 13:14:15#, vbThursday, vbFirstJan1)
    M1828 = R
    Exit Function
EH:
    M1828 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1829() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = DatePart("s", #2/29/2000 0:00:00#) & "|" & DatePart("s", #2/29/2000 0:00:00#, vbMonday) & "|" & DatePart("s", #2/29/2000 0:00:00#, vbSunday, vbFirstFourDays) & "|" & DatePart("s", #2/29/2000 0:00:00#, vbSaturday, vbFirstFullWeek) & "|" & DatePart("s", #2/29/2000 0:00:00#, vbThursday, vbFirstJan1)
    M1829 = R
    Exit Function
EH:
    M1829 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1830() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#1/1/2000#) & Weekday(#1/1/2000#, vbMonday) & Weekday(#1/1/2000#, vbSaturday) & Weekday(#1/1/2000#, 0)
    M1830 = R
    Exit Function
EH:
    M1830 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1831() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#1/2/2000#) & Weekday(#1/2/2000#, vbMonday) & Weekday(#1/2/2000#, vbSaturday) & Weekday(#1/2/2000#, 0)
    M1831 = R
    Exit Function
EH:
    M1831 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1832() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#12/31/1899#) & Weekday(#12/31/1899#, vbMonday) & Weekday(#12/31/1899#, vbSaturday) & Weekday(#12/31/1899#, 0)
    M1832 = R
    Exit Function
EH:
    M1832 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1833() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#12/30/1899#) & Weekday(#12/30/1899#, vbMonday) & Weekday(#12/30/1899#, vbSaturday) & Weekday(#12/30/1899#, 0)
    M1833 = R
    Exit Function
EH:
    M1833 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1834() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#1/1/100#) & Weekday(#1/1/100#, vbMonday) & Weekday(#1/1/100#, vbSaturday) & Weekday(#1/1/100#, 0)
    M1834 = R
    Exit Function
EH:
    M1834 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1835() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#12/31/9999#) & Weekday(#12/31/9999#, vbMonday) & Weekday(#12/31/9999#, vbSaturday) & Weekday(#12/31/9999#, 0)
    M1835 = R
    Exit Function
EH:
    M1835 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1836() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#1/1/2000#, 8)
    M1836 = R
    Exit Function
EH:
    M1836 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1837() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(#1/1/2000#, -1)
    M1837 = R
    Exit Function
EH:
    M1837 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1838() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday("abc")
    M1838 = R
    Exit Function
EH:
    M1838 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1839() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday("2000-01-01")
    M1839 = R
    Exit Function
EH:
    M1839 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1840() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(0)
    M1840 = R
    Exit Function
EH:
    M1840 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1841() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(-1)
    M1841 = R
    Exit Function
EH:
    M1841 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1842() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Weekday(1.9)
    M1842 = R
    Exit Function
EH:
    M1842 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1843() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year(0) & "|" & Month(0) & "|" & Day(0) & "|" & Hour(0) & "|" & Minute(0) & "|" & Second(0)
    M1843 = R
    Exit Function
EH:
    M1843 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1844() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year(-0.5) & "|" & Month(-0.5) & "|" & Day(-0.5) & "|" & Hour(-0.5) & "|" & Minute(-0.5) & "|" & Second(-0.5)
    M1844 = R
    Exit Function
EH:
    M1844 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1845() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Hour(0.999999) & "|" & Minute(0.999999) & "|" & Second(0.999999) & "|" & Second(1 / 86400 * 0.5) & "|" & Second(1 / 86400 * 0.49) & "|" & Second(59.5 / 86400)
    M1845 = R
    Exit Function
EH:
    M1845 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1846() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year("1/2/03") & "|" & Year("1/2/29") & "|" & Year("1/2/30") & "|" & Year("1/2/49") & "|" & Year("1/2/50") & "|" & Year("1/2/99")
    M1846 = R
    Exit Function
EH:
    M1846 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1847() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year("abc")
    M1847 = R
    Exit Function
EH:
    M1847 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1848() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year(3000000)
    M1848 = R
    Exit Function
EH:
    M1848 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1849() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year(2958465) & Year(2958466)
    M1849 = R
    Exit Function
EH:
    M1849 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1850() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Year(-657434) & Year(-657435)
    M1850 = R
    Exit Function
EH:
    M1850 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1851() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Month("Jan 5 2001") & Day("5 Jan 2001") & Month("January 2001")
    M1851 = R
    Exit Function
EH:
    M1851 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1852() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = MonthName(1) & MonthName(12, True) & MonthName(6, False)
    M1852 = R
    Exit Function
EH:
    M1852 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1853() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = MonthName(0)
    M1853 = R
    Exit Function
EH:
    M1853 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1854() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = MonthName(13)
    M1854 = R
    Exit Function
EH:
    M1854 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1855() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = MonthName(1.5)
    M1855 = R
    Exit Function
EH:
    M1855 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1856() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = MonthName(Null)
    M1856 = R
    Exit Function
EH:
    M1856 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1857() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = WeekdayName(1) & WeekdayName(7, True) & WeekdayName(1, False, vbMonday) & WeekdayName(1, True, vbSaturday)
    M1857 = R
    Exit Function
EH:
    M1857 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1858() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = WeekdayName(0)
    M1858 = R
    Exit Function
EH:
    M1858 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1859() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = WeekdayName(8)
    M1859 = R
    Exit Function
EH:
    M1859 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1860() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = WeekdayName(1, , 8)
    M1860 = R
    Exit Function
EH:
    M1860 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1861() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = WeekdayName(1, , -1)
    M1861 = R
    Exit Function
EH:
    M1861 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1862() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = TypeName(Now) & TypeName(Date) & TypeName(Time) & TypeName(Timer) & (Timer >= 0) & (Timer < 86400) & (Int(Now) = Date) & (Date + Time - Now < 0.00001)
    M1862 = R
    Exit Function
EH:
    M1862 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1863() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(Date) = Format(Date, "Short Date"))) & Dump(fV(CStr(Time) = Format(Time, "Long Time")))
    M1863 = R
    Exit Function
EH:
    M1863 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1864() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Weekday(Null)))
    M1864 = R
    Exit Function
EH:
    M1864 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1865() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Year(Null)))
    M1865 = R
    Exit Function
EH:
    M1865 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1866() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hour("13:45")))
    M1866 = R
    Exit Function
EH:
    M1866 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1867() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Minute("1:45 pm")))
    M1867 = R
    Exit Function
EH:
    M1867 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1868() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(2000, 2, 30)))
    M1868 = R
    Exit Function
EH:
    M1868 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1869() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(2000, 0, 0)))
    M1869 = R
    Exit Function
EH:
    M1869 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1870() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(2000, 14, -5)))
    M1870 = R
    Exit Function
EH:
    M1870 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1871() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(99, 1, 1)))
    M1871 = R
    Exit Function
EH:
    M1871 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1872() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(0, 1, 1)))
    M1872 = R
    Exit Function
EH:
    M1872 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1873() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(30, 1, 1)))
    M1873 = R
    Exit Function
EH:
    M1873 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1874() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(29, 1, 1)))
    M1874 = R
    Exit Function
EH:
    M1874 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1875() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(100, 1, 1)))
    M1875 = R
    Exit Function
EH:
    M1875 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1876() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(9999, 12, 31)))
    M1876 = R
    Exit Function
EH:
    M1876 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1877() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(10000, 1, 1)))
    M1877 = R
    Exit Function
EH:
    M1877 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1878() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(-1, 1, 1)))
    M1878 = R
    Exit Function
EH:
    M1878 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1879() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(1900, 1, 0)))
    M1879 = R
    Exit Function
EH:
    M1879 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1880() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(1, 1, 1)))
    M1880 = R
    Exit Function
EH:
    M1880 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1881() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(2000.5, 1.5, 2.5)))
    M1881 = R
    Exit Function
EH:
    M1881 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1882() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(2000, 1, 1 + 1E5)))
    M1882 = R
    Exit Function
EH:
    M1882 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1883() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(Null, 1, 1)))
    M1883 = R
    Exit Function
EH:
    M1883 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1884() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial("2000", "2", "3")))
    M1884 = R
    Exit Function
EH:
    M1884 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1885() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateSerial(32768, 1, 1)))
    M1885 = R
    Exit Function
EH:
    M1885 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1886() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(25, 70, 70)))
    M1886 = R
    Exit Function
EH:
    M1886 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1887() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(-1, 0, 0)))
    M1887 = R
    Exit Function
EH:
    M1887 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1888() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(0, 0, -1)))
    M1888 = R
    Exit Function
EH:
    M1888 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1889() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(24, 0, 0)))
    M1889 = R
    Exit Function
EH:
    M1889 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1890() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(12, 30, 0)))
    M1890 = R
    Exit Function
EH:
    M1890 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1891() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(0.5, 0.5, 0.5)))
    M1891 = R
    Exit Function
EH:
    M1891 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1892() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(32767, 0, 0)))
    M1892 = R
    Exit Function
EH:
    M1892 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1893() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(0, 32767, 0)))
    M1893 = R
    Exit Function
EH:
    M1893 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1894() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeSerial(40000, 0, 0)))
    M1894 = R
    Exit Function
EH:
    M1894 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1895() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeValue("1:02:03 PM")))
    M1895 = R
    Exit Function
EH:
    M1895 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1896() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeValue("13:02")))
    M1896 = R
    Exit Function
EH:
    M1896 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1897() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeValue("1/2/2000 3:04")))
    M1897 = R
    Exit Function
EH:
    M1897 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1898() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeValue("abc")))
    M1898 = R
    Exit Function
EH:
    M1898 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1899() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TimeValue("25:00")))
    M1899 = R
    Exit Function
EH:
    M1899 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1900() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("1/2/2003")))
    M1900 = R
    Exit Function
EH:
    M1900 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1901() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("Jan 2 2003")))
    M1901 = R
    Exit Function
EH:
    M1901 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1902() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("2 Jan 2003 5:00")))
    M1902 = R
    Exit Function
EH:
    M1902 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1903() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("2003-01-02")))
    M1903 = R
    Exit Function
EH:
    M1903 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1904() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("1/2")))
    M1904 = R
    Exit Function
EH:
    M1904 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1905() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("abc")))
    M1905 = R
    Exit Function
EH:
    M1905 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1906() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("12:30")))
    M1906 = R
    Exit Function
EH:
    M1906 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1907() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("2/30/2000")))
    M1907 = R
    Exit Function
EH:
    M1907 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1908() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue("13/1/2000")))
    M1908 = R
    Exit Function
EH:
    M1908 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1909() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue(Null)))
    M1909 = R
    Exit Function
EH:
    M1909 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1910() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue(36526)))
    M1910 = R
    Exit Function
EH:
    M1910 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1911() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(DateValue(#1/2/2000 3:04:00 AM#)))
    M1911 = R
    Exit Function
EH:
    M1911 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1912() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("1/2/2003")))
    M1912 = R
    Exit Function
EH:
    M1912 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1913() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("13/13/2003")))
    M1913 = R
    Exit Function
EH:
    M1913 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1914() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate(Null)))
    M1914 = R
    Exit Function
EH:
    M1914 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1915() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate(5)))
    M1915 = R
    Exit Function
EH:
    M1915 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1916() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate(Empty)))
    M1916 = R
    Exit Function
EH:
    M1916 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1917() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("Jan")))
    M1917 = R
    Exit Function
EH:
    M1917 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1918() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("12:60")))
    M1918 = R
    Exit Function
EH:
    M1918 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1919() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("2000-01-01")))
    M1919 = R
    Exit Function
EH:
    M1919 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1920() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("1/1/100")))
    M1920 = R
    Exit Function
EH:
    M1920 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1921() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate("Feb 30")))
    M1921 = R
    Exit Function
EH:
    M1921 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1922() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("2000-01-01") + 0.5))
    M1922 = R
    Exit Function
EH:
    M1922 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1923() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1/1/2000") - CDate("12/31/1999")))
    M1923 = R
    Exit Function
EH:
    M1923 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1924() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# + 1))
    M1924 = R
    Exit Function
EH:
    M1924 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1925() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# - #1/2/2000#))
    M1925 = R
    Exit Function
EH:
    M1925 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1926() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# & "x"))
    M1926 = R
    Exit Function
EH:
    M1926 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1927() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# + #1/1/2000#))
    M1927 = R
    Exit Function
EH:
    M1927 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1928() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# * 2))
    M1928 = R
    Exit Function
EH:
    M1928 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1929() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(#1/1/2000#)))
    M1929 = R
    Exit Function
EH:
    M1929 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1930() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(2958465.99999)))
    M1930 = R
    Exit Function
EH:
    M1930 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1931() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(2958466)))
    M1931 = R
    Exit Function
EH:
    M1931 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1932() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(-657434.5)))
    M1932 = R
    Exit Function
EH:
    M1932 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1933() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(-657435)))
    M1933 = R
    Exit Function
EH:
    M1933 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1934() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("12/31/9999 23:59:59")))
    M1934 = R
    Exit Function
EH:
    M1934 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1935() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1/1/100")))
    M1935 = R
    Exit Function
EH:
    M1935 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1936() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("0:00")))
    M1936 = R
    Exit Function
EH:
    M1936 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1937() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("24:00")))
    M1937 = R
    Exit Function
EH:
    M1937 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1938() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("1/1/2000 25:00")))
    M1938 = R
    Exit Function
EH:
    M1938 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1939() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate("12:30:45.5")))
    M1939 = R
    Exit Function
EH:
    M1939 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1940() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(1E-10)))
    M1940 = R
    Exit Function
EH:
    M1940 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1941() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(0.99999999999)))
    M1941 = R
    Exit Function
EH:
    M1941 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1942() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UCase(s)))
    M1942 = R
    Exit Function
EH:
    M1942 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1943() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UCase(s)))
    M1943 = R
    Exit Function
EH:
    M1943 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1944() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UCase(s)))
    M1944 = R
    Exit Function
EH:
    M1944 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1945() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UCase(s)))
    M1945 = R
    Exit Function
EH:
    M1945 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1946() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UCase(s)))
    M1946 = R
    Exit Function
EH:
    M1946 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1947() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UCase(s)))
    M1947 = R
    Exit Function
EH:
    M1947 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1948() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UCase(s)))
    M1948 = R
    Exit Function
EH:
    M1948 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1949() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UCase(s)))
    M1949 = R
    Exit Function
EH:
    M1949 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1950() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UCase(s)))
    M1950 = R
    Exit Function
EH:
    M1950 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1951() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UCase(s)))
    M1951 = R
    Exit Function
EH:
    M1951 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1952() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UCase(s)))
    M1952 = R
    Exit Function
EH:
    M1952 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1953() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UCase(s)))
    M1953 = R
    Exit Function
EH:
    M1953 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1954() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UCase(s)))
    M1954 = R
    Exit Function
EH:
    M1954 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1955() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UCase(s)))
    M1955 = R
    Exit Function
EH:
    M1955 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1956() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UCase(s)))
    M1956 = R
    Exit Function
EH:
    M1956 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1957() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UCase(s)))
    M1957 = R
    Exit Function
EH:
    M1957 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1958() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UCase(s)))
    M1958 = R
    Exit Function
EH:
    M1958 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1959() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UCase(s)))
    M1959 = R
    Exit Function
EH:
    M1959 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1960() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UCase(s)))
    M1960 = R
    Exit Function
EH:
    M1960 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1961() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UCase(s)))
    M1961 = R
    Exit Function
EH:
    M1961 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1962() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(LCase(s)))
    M1962 = R
    Exit Function
EH:
    M1962 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1963() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(LCase(s)))
    M1963 = R
    Exit Function
EH:
    M1963 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1964() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(LCase(s)))
    M1964 = R
    Exit Function
EH:
    M1964 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1965() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(LCase(s)))
    M1965 = R
    Exit Function
EH:
    M1965 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1966() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(LCase(s)))
    M1966 = R
    Exit Function
EH:
    M1966 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1967() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(LCase(s)))
    M1967 = R
    Exit Function
EH:
    M1967 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1968() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(LCase(s)))
    M1968 = R
    Exit Function
EH:
    M1968 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1969() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(LCase(s)))
    M1969 = R
    Exit Function
EH:
    M1969 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1970() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(LCase(s)))
    M1970 = R
    Exit Function
EH:
    M1970 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1971() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(LCase(s)))
    M1971 = R
    Exit Function
EH:
    M1971 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1972() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(LCase(s)))
    M1972 = R
    Exit Function
EH:
    M1972 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1973() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(LCase(s)))
    M1973 = R
    Exit Function
EH:
    M1973 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1974() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(LCase(s)))
    M1974 = R
    Exit Function
EH:
    M1974 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1975() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(LCase(s)))
    M1975 = R
    Exit Function
EH:
    M1975 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1976() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(LCase(s)))
    M1976 = R
    Exit Function
EH:
    M1976 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1977() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(LCase(s)))
    M1977 = R
    Exit Function
EH:
    M1977 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1978() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(LCase(s)))
    M1978 = R
    Exit Function
EH:
    M1978 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1979() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(LCase(s)))
    M1979 = R
    Exit Function
EH:
    M1979 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1980() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(LCase(s)))
    M1980 = R
    Exit Function
EH:
    M1980 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1981() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(LCase(s)))
    M1981 = R
    Exit Function
EH:
    M1981 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1982() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Trim(s)))
    M1982 = R
    Exit Function
EH:
    M1982 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1983() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Trim(s)))
    M1983 = R
    Exit Function
EH:
    M1983 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1984() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Trim(s)))
    M1984 = R
    Exit Function
EH:
    M1984 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1985() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Trim(s)))
    M1985 = R
    Exit Function
EH:
    M1985 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1986() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Trim(s)))
    M1986 = R
    Exit Function
EH:
    M1986 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1987() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Trim(s)))
    M1987 = R
    Exit Function
EH:
    M1987 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1988() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Trim(s)))
    M1988 = R
    Exit Function
EH:
    M1988 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1989() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Trim(s)))
    M1989 = R
    Exit Function
EH:
    M1989 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1990() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Trim(s)))
    M1990 = R
    Exit Function
EH:
    M1990 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1991() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Trim(s)))
    M1991 = R
    Exit Function
EH:
    M1991 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1992() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Trim(s)))
    M1992 = R
    Exit Function
EH:
    M1992 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1993() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Trim(s)))
    M1993 = R
    Exit Function
EH:
    M1993 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1994() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Trim(s)))
    M1994 = R
    Exit Function
EH:
    M1994 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1995() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Trim(s)))
    M1995 = R
    Exit Function
EH:
    M1995 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1996() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Trim(s)))
    M1996 = R
    Exit Function
EH:
    M1996 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1997() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Trim(s)))
    M1997 = R
    Exit Function
EH:
    M1997 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1998() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Trim(s)))
    M1998 = R
    Exit Function
EH:
    M1998 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M1999() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Trim(s)))
    M1999 = R
    Exit Function
EH:
    M1999 = "#ERR " & Err.Number & " " & Err.Description
End Function
