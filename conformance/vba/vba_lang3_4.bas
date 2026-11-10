Attribute VB_Name = "VbaLang3_4"
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

Public Function LangThree4(ByVal k As Long) As Variant
    Select Case (k - 4000) \ 100
    Case 0: LangThree4 = MC0(k)
    Case 1: LangThree4 = MC1(k)
    Case 2: LangThree4 = MC2(k)
    Case 3: LangThree4 = MC3(k)
    Case 4: LangThree4 = MC4(k)
    Case 5: LangThree4 = MC5(k)
    Case 6: LangThree4 = MC6(k)
    Case 7: LangThree4 = MC7(k)
    Case 8: LangThree4 = MC8(k)
    Case 9: LangThree4 = MC9(k)
    End Select
End Function

Private Function MC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4000: MC0 = M4000()
    Case 4001: MC0 = M4001()
    Case 4002: MC0 = M4002()
    Case 4003: MC0 = M4003()
    Case 4004: MC0 = M4004()
    Case 4005: MC0 = M4005()
    Case 4006: MC0 = M4006()
    Case 4007: MC0 = M4007()
    Case 4008: MC0 = M4008()
    Case 4009: MC0 = M4009()
    Case 4010: MC0 = M4010()
    Case 4011: MC0 = M4011()
    Case 4012: MC0 = M4012()
    Case 4013: MC0 = M4013()
    Case 4014: MC0 = M4014()
    Case 4015: MC0 = M4015()
    Case 4016: MC0 = M4016()
    Case 4017: MC0 = M4017()
    Case 4018: MC0 = M4018()
    Case 4019: MC0 = M4019()
    Case 4020: MC0 = M4020()
    Case 4021: MC0 = M4021()
    Case 4022: MC0 = M4022()
    Case 4023: MC0 = M4023()
    Case 4024: MC0 = M4024()
    Case 4025: MC0 = M4025()
    Case 4026: MC0 = M4026()
    Case 4027: MC0 = M4027()
    Case 4028: MC0 = M4028()
    Case 4029: MC0 = M4029()
    Case 4030: MC0 = M4030()
    Case 4031: MC0 = M4031()
    Case 4032: MC0 = M4032()
    Case 4033: MC0 = M4033()
    Case 4034: MC0 = M4034()
    Case 4035: MC0 = M4035()
    Case 4036: MC0 = M4036()
    Case 4037: MC0 = M4037()
    Case 4038: MC0 = M4038()
    Case 4039: MC0 = M4039()
    Case 4040: MC0 = M4040()
    Case 4041: MC0 = M4041()
    Case 4042: MC0 = M4042()
    Case 4043: MC0 = M4043()
    Case 4044: MC0 = M4044()
    Case 4045: MC0 = M4045()
    Case 4046: MC0 = M4046()
    Case 4047: MC0 = M4047()
    Case 4048: MC0 = M4048()
    Case 4049: MC0 = M4049()
    Case 4050: MC0 = M4050()
    Case 4051: MC0 = M4051()
    Case 4052: MC0 = M4052()
    Case 4053: MC0 = M4053()
    Case 4054: MC0 = M4054()
    Case 4055: MC0 = M4055()
    Case 4056: MC0 = M4056()
    Case 4057: MC0 = M4057()
    Case 4058: MC0 = M4058()
    Case 4059: MC0 = M4059()
    Case 4060: MC0 = M4060()
    Case 4061: MC0 = M4061()
    Case 4062: MC0 = M4062()
    Case 4063: MC0 = M4063()
    Case 4064: MC0 = M4064()
    Case 4065: MC0 = M4065()
    Case 4066: MC0 = M4066()
    Case 4067: MC0 = M4067()
    Case 4068: MC0 = M4068()
    Case 4069: MC0 = M4069()
    Case 4070: MC0 = M4070()
    Case 4071: MC0 = M4071()
    Case 4072: MC0 = M4072()
    Case 4073: MC0 = M4073()
    Case 4074: MC0 = M4074()
    Case 4075: MC0 = M4075()
    Case 4076: MC0 = M4076()
    Case 4077: MC0 = M4077()
    Case 4078: MC0 = M4078()
    Case 4079: MC0 = M4079()
    Case 4080: MC0 = M4080()
    Case 4081: MC0 = M4081()
    Case 4082: MC0 = M4082()
    Case 4083: MC0 = M4083()
    Case 4084: MC0 = M4084()
    Case 4085: MC0 = M4085()
    Case 4086: MC0 = M4086()
    Case 4087: MC0 = M4087()
    Case 4088: MC0 = M4088()
    Case 4089: MC0 = M4089()
    Case 4090: MC0 = M4090()
    Case 4091: MC0 = M4091()
    Case 4092: MC0 = M4092()
    Case 4093: MC0 = M4093()
    Case 4094: MC0 = M4094()
    Case 4095: MC0 = M4095()
    Case 4096: MC0 = M4096()
    Case 4097: MC0 = M4097()
    Case 4098: MC0 = M4098()
    Case 4099: MC0 = M4099()
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4100: MC1 = M4100()
    Case 4101: MC1 = M4101()
    Case 4102: MC1 = M4102()
    Case 4103: MC1 = M4103()
    Case 4104: MC1 = M4104()
    Case 4105: MC1 = M4105()
    Case 4106: MC1 = M4106()
    Case 4107: MC1 = M4107()
    Case 4108: MC1 = M4108()
    Case 4109: MC1 = M4109()
    Case 4110: MC1 = M4110()
    Case 4111: MC1 = M4111()
    Case 4112: MC1 = M4112()
    Case 4113: MC1 = M4113()
    Case 4114: MC1 = M4114()
    Case 4115: MC1 = M4115()
    Case 4116: MC1 = M4116()
    Case 4117: MC1 = M4117()
    Case 4118: MC1 = M4118()
    Case 4119: MC1 = M4119()
    Case 4120: MC1 = M4120()
    Case 4121: MC1 = M4121()
    Case 4122: MC1 = M4122()
    Case 4123: MC1 = M4123()
    Case 4124: MC1 = M4124()
    Case 4125: MC1 = M4125()
    Case 4126: MC1 = M4126()
    Case 4127: MC1 = M4127()
    Case 4128: MC1 = M4128()
    Case 4129: MC1 = M4129()
    Case 4130: MC1 = M4130()
    Case 4131: MC1 = M4131()
    Case 4132: MC1 = M4132()
    Case 4133: MC1 = M4133()
    Case 4134: MC1 = M4134()
    Case 4135: MC1 = M4135()
    Case 4136: MC1 = M4136()
    Case 4137: MC1 = M4137()
    Case 4138: MC1 = M4138()
    Case 4139: MC1 = M4139()
    Case 4140: MC1 = M4140()
    Case 4141: MC1 = M4141()
    Case 4142: MC1 = M4142()
    Case 4143: MC1 = M4143()
    Case 4144: MC1 = M4144()
    Case 4145: MC1 = M4145()
    Case 4146: MC1 = M4146()
    Case 4147: MC1 = M4147()
    Case 4148: MC1 = M4148()
    Case 4149: MC1 = M4149()
    Case 4150: MC1 = M4150()
    Case 4151: MC1 = M4151()
    Case 4152: MC1 = M4152()
    Case 4153: MC1 = M4153()
    Case 4154: MC1 = M4154()
    Case 4155: MC1 = M4155()
    Case 4156: MC1 = M4156()
    Case 4157: MC1 = M4157()
    Case 4158: MC1 = M4158()
    Case 4159: MC1 = M4159()
    Case 4160: MC1 = M4160()
    Case 4161: MC1 = M4161()
    Case 4162: MC1 = M4162()
    Case 4163: MC1 = M4163()
    Case 4164: MC1 = M4164()
    Case 4165: MC1 = M4165()
    Case 4166: MC1 = M4166()
    Case 4167: MC1 = M4167()
    Case 4168: MC1 = M4168()
    Case 4169: MC1 = M4169()
    Case 4170: MC1 = M4170()
    Case 4171: MC1 = M4171()
    Case 4172: MC1 = M4172()
    Case 4173: MC1 = M4173()
    Case 4174: MC1 = M4174()
    Case 4175: MC1 = M4175()
    Case 4176: MC1 = M4176()
    Case 4177: MC1 = M4177()
    Case 4178: MC1 = M4178()
    Case 4179: MC1 = M4179()
    Case 4180: MC1 = M4180()
    Case 4181: MC1 = M4181()
    Case 4182: MC1 = M4182()
    Case 4183: MC1 = M4183()
    Case 4184: MC1 = M4184()
    Case 4185: MC1 = M4185()
    Case 4186: MC1 = M4186()
    Case 4187: MC1 = M4187()
    Case 4188: MC1 = M4188()
    Case 4189: MC1 = M4189()
    Case 4190: MC1 = M4190()
    Case 4191: MC1 = M4191()
    Case 4192: MC1 = M4192()
    Case 4193: MC1 = M4193()
    Case 4194: MC1 = M4194()
    Case 4195: MC1 = M4195()
    Case 4196: MC1 = M4196()
    Case 4197: MC1 = M4197()
    Case 4198: MC1 = M4198()
    Case 4199: MC1 = M4199()
    End Select
    Exit Function
EH:
    MC1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4200: MC2 = M4200()
    Case 4201: MC2 = M4201()
    Case 4202: MC2 = M4202()
    Case 4203: MC2 = M4203()
    Case 4204: MC2 = M4204()
    Case 4205: MC2 = M4205()
    Case 4206: MC2 = M4206()
    Case 4207: MC2 = M4207()
    Case 4208: MC2 = M4208()
    Case 4209: MC2 = M4209()
    Case 4210: MC2 = M4210()
    Case 4211: MC2 = M4211()
    Case 4212: MC2 = M4212()
    Case 4213: MC2 = M4213()
    Case 4214: MC2 = M4214()
    Case 4215: MC2 = M4215()
    Case 4216: MC2 = M4216()
    Case 4217: MC2 = M4217()
    Case 4218: MC2 = M4218()
    Case 4219: MC2 = M4219()
    Case 4220: MC2 = M4220()
    Case 4221: MC2 = M4221()
    Case 4222: MC2 = M4222()
    Case 4223: MC2 = M4223()
    Case 4224: MC2 = M4224()
    Case 4225: MC2 = M4225()
    Case 4226: MC2 = M4226()
    Case 4227: MC2 = M4227()
    Case 4228: MC2 = M4228()
    Case 4229: MC2 = M4229()
    Case 4230: MC2 = M4230()
    Case 4231: MC2 = M4231()
    Case 4232: MC2 = M4232()
    Case 4233: MC2 = M4233()
    Case 4234: MC2 = M4234()
    Case 4235: MC2 = M4235()
    Case 4236: MC2 = M4236()
    Case 4237: MC2 = M4237()
    Case 4238: MC2 = M4238()
    Case 4239: MC2 = M4239()
    Case 4240: MC2 = M4240()
    Case 4241: MC2 = M4241()
    Case 4242: MC2 = M4242()
    Case 4243: MC2 = M4243()
    Case 4244: MC2 = M4244()
    Case 4245: MC2 = M4245()
    Case 4246: MC2 = M4246()
    Case 4247: MC2 = M4247()
    Case 4248: MC2 = M4248()
    Case 4249: MC2 = M4249()
    Case 4250: MC2 = M4250()
    Case 4251: MC2 = M4251()
    Case 4252: MC2 = M4252()
    Case 4253: MC2 = M4253()
    Case 4254: MC2 = M4254()
    Case 4255: MC2 = M4255()
    Case 4256: MC2 = M4256()
    Case 4257: MC2 = M4257()
    Case 4258: MC2 = M4258()
    Case 4259: MC2 = M4259()
    Case 4260: MC2 = M4260()
    Case 4261: MC2 = M4261()
    Case 4262: MC2 = M4262()
    Case 4263: MC2 = M4263()
    Case 4264: MC2 = M4264()
    Case 4265: MC2 = M4265()
    Case 4266: MC2 = M4266()
    Case 4267: MC2 = M4267()
    Case 4268: MC2 = M4268()
    Case 4269: MC2 = M4269()
    Case 4270: MC2 = M4270()
    Case 4271: MC2 = M4271()
    Case 4272: MC2 = M4272()
    Case 4273: MC2 = M4273()
    Case 4274: MC2 = M4274()
    Case 4275: MC2 = M4275()
    Case 4276: MC2 = M4276()
    Case 4277: MC2 = M4277()
    Case 4278: MC2 = M4278()
    Case 4279: MC2 = M4279()
    Case 4280: MC2 = M4280()
    Case 4281: MC2 = M4281()
    Case 4282: MC2 = M4282()
    Case 4283: MC2 = M4283()
    Case 4284: MC2 = M4284()
    Case 4285: MC2 = M4285()
    Case 4286: MC2 = M4286()
    Case 4287: MC2 = M4287()
    Case 4288: MC2 = M4288()
    Case 4289: MC2 = M4289()
    Case 4290: MC2 = M4290()
    Case 4291: MC2 = M4291()
    Case 4292: MC2 = M4292()
    Case 4293: MC2 = M4293()
    Case 4294: MC2 = M4294()
    Case 4295: MC2 = M4295()
    Case 4296: MC2 = M4296()
    Case 4297: MC2 = M4297()
    Case 4298: MC2 = M4298()
    Case 4299: MC2 = M4299()
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC3(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4300: MC3 = M4300()
    Case 4301: MC3 = M4301()
    Case 4302: MC3 = M4302()
    Case 4303: MC3 = M4303()
    Case 4304: MC3 = M4304()
    Case 4305: MC3 = M4305()
    Case 4306: MC3 = M4306()
    Case 4307: MC3 = M4307()
    Case 4308: MC3 = M4308()
    Case 4309: MC3 = M4309()
    Case 4310: MC3 = M4310()
    Case 4311: MC3 = M4311()
    Case 4312: MC3 = M4312()
    Case 4313: MC3 = M4313()
    Case 4314: MC3 = M4314()
    Case 4315: MC3 = M4315()
    Case 4316: MC3 = M4316()
    Case 4317: MC3 = M4317()
    Case 4318: MC3 = M4318()
    Case 4319: MC3 = M4319()
    Case 4320: MC3 = M4320()
    Case 4321: MC3 = M4321()
    Case 4322: MC3 = M4322()
    Case 4323: MC3 = M4323()
    Case 4324: MC3 = M4324()
    Case 4325: MC3 = M4325()
    Case 4326: MC3 = M4326()
    Case 4327: MC3 = M4327()
    Case 4328: MC3 = M4328()
    Case 4329: MC3 = M4329()
    Case 4330: MC3 = M4330()
    Case 4331: MC3 = M4331()
    Case 4332: MC3 = M4332()
    Case 4333: MC3 = M4333()
    Case 4334: MC3 = M4334()
    Case 4335: MC3 = M4335()
    Case 4336: MC3 = M4336()
    Case 4337: MC3 = M4337()
    Case 4338: MC3 = M4338()
    Case 4339: MC3 = M4339()
    Case 4340: MC3 = M4340()
    Case 4341: MC3 = M4341()
    Case 4342: MC3 = M4342()
    Case 4343: MC3 = M4343()
    Case 4344: MC3 = M4344()
    Case 4345: MC3 = M4345()
    Case 4346: MC3 = M4346()
    Case 4347: MC3 = M4347()
    Case 4348: MC3 = M4348()
    Case 4349: MC3 = M4349()
    Case 4350: MC3 = M4350()
    Case 4351: MC3 = M4351()
    Case 4352: MC3 = M4352()
    Case 4353: MC3 = M4353()
    Case 4354: MC3 = M4354()
    Case 4355: MC3 = M4355()
    Case 4356: MC3 = M4356()
    Case 4357: MC3 = M4357()
    Case 4358: MC3 = M4358()
    Case 4359: MC3 = M4359()
    Case 4360: MC3 = M4360()
    Case 4361: MC3 = M4361()
    Case 4362: MC3 = M4362()
    Case 4363: MC3 = M4363()
    Case 4364: MC3 = M4364()
    Case 4365: MC3 = M4365()
    Case 4366: MC3 = M4366()
    Case 4367: MC3 = M4367()
    Case 4368: MC3 = M4368()
    Case 4369: MC3 = M4369()
    Case 4370: MC3 = M4370()
    Case 4371: MC3 = M4371()
    Case 4372: MC3 = M4372()
    Case 4373: MC3 = M4373()
    Case 4374: MC3 = M4374()
    Case 4375: MC3 = M4375()
    Case 4376: MC3 = M4376()
    Case 4377: MC3 = M4377()
    Case 4378: MC3 = M4378()
    Case 4379: MC3 = M4379()
    Case 4380: MC3 = M4380()
    Case 4381: MC3 = M4381()
    Case 4382: MC3 = M4382()
    Case 4383: MC3 = M4383()
    Case 4384: MC3 = M4384()
    Case 4385: MC3 = M4385()
    Case 4386: MC3 = M4386()
    Case 4387: MC3 = M4387()
    Case 4388: MC3 = M4388()
    Case 4389: MC3 = M4389()
    Case 4390: MC3 = M4390()
    Case 4391: MC3 = M4391()
    Case 4392: MC3 = M4392()
    Case 4393: MC3 = M4393()
    Case 4394: MC3 = M4394()
    Case 4395: MC3 = M4395()
    Case 4396: MC3 = M4396()
    Case 4397: MC3 = M4397()
    Case 4398: MC3 = M4398()
    Case 4399: MC3 = M4399()
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4400: MC4 = M4400()
    Case 4401: MC4 = M4401()
    Case 4402: MC4 = M4402()
    Case 4403: MC4 = M4403()
    Case 4404: MC4 = M4404()
    Case 4405: MC4 = M4405()
    Case 4406: MC4 = M4406()
    Case 4407: MC4 = M4407()
    Case 4408: MC4 = M4408()
    Case 4409: MC4 = M4409()
    Case 4410: MC4 = M4410()
    Case 4411: MC4 = M4411()
    Case 4412: MC4 = M4412()
    Case 4413: MC4 = M4413()
    Case 4414: MC4 = M4414()
    Case 4415: MC4 = M4415()
    Case 4416: MC4 = M4416()
    Case 4417: MC4 = M4417()
    Case 4418: MC4 = M4418()
    Case 4419: MC4 = M4419()
    Case 4420: MC4 = M4420()
    Case 4421: MC4 = M4421()
    Case 4422: MC4 = M4422()
    Case 4423: MC4 = M4423()
    Case 4424: MC4 = M4424()
    Case 4425: MC4 = M4425()
    Case 4426: MC4 = M4426()
    Case 4427: MC4 = M4427()
    Case 4428: MC4 = M4428()
    Case 4429: MC4 = M4429()
    Case 4430: MC4 = M4430()
    Case 4431: MC4 = M4431()
    Case 4432: MC4 = M4432()
    Case 4433: MC4 = M4433()
    Case 4434: MC4 = M4434()
    Case 4435: MC4 = M4435()
    Case 4436: MC4 = M4436()
    Case 4437: MC4 = M4437()
    Case 4438: MC4 = M4438()
    Case 4439: MC4 = M4439()
    Case 4440: MC4 = M4440()
    Case 4441: MC4 = M4441()
    Case 4442: MC4 = M4442()
    Case 4443: MC4 = M4443()
    Case 4444: MC4 = M4444()
    Case 4445: MC4 = M4445()
    Case 4446: MC4 = M4446()
    Case 4447: MC4 = M4447()
    Case 4448: MC4 = M4448()
    Case 4449: MC4 = M4449()
    Case 4450: MC4 = M4450()
    Case 4451: MC4 = M4451()
    Case 4452: MC4 = M4452()
    Case 4453: MC4 = M4453()
    Case 4454: MC4 = M4454()
    Case 4455: MC4 = M4455()
    Case 4456: MC4 = M4456()
    Case 4457: MC4 = M4457()
    Case 4458: MC4 = M4458()
    Case 4459: MC4 = M4459()
    Case 4460: MC4 = M4460()
    Case 4461: MC4 = M4461()
    Case 4462: MC4 = M4462()
    Case 4463: MC4 = M4463()
    Case 4464: MC4 = M4464()
    Case 4465: MC4 = M4465()
    Case 4466: MC4 = M4466()
    Case 4467: MC4 = M4467()
    Case 4468: MC4 = M4468()
    Case 4469: MC4 = M4469()
    Case 4470: MC4 = M4470()
    Case 4471: MC4 = M4471()
    Case 4472: MC4 = M4472()
    Case 4473: MC4 = M4473()
    Case 4474: MC4 = M4474()
    Case 4475: MC4 = M4475()
    Case 4476: MC4 = M4476()
    Case 4477: MC4 = M4477()
    Case 4478: MC4 = M4478()
    Case 4479: MC4 = M4479()
    Case 4480: MC4 = M4480()
    Case 4481: MC4 = M4481()
    Case 4482: MC4 = M4482()
    Case 4483: MC4 = M4483()
    Case 4484: MC4 = M4484()
    Case 4485: MC4 = M4485()
    Case 4486: MC4 = M4486()
    Case 4487: MC4 = M4487()
    Case 4488: MC4 = M4488()
    Case 4489: MC4 = M4489()
    Case 4490: MC4 = M4490()
    Case 4491: MC4 = M4491()
    Case 4492: MC4 = M4492()
    Case 4493: MC4 = M4493()
    Case 4494: MC4 = M4494()
    Case 4495: MC4 = M4495()
    Case 4496: MC4 = M4496()
    Case 4497: MC4 = M4497()
    Case 4498: MC4 = M4498()
    Case 4499: MC4 = M4499()
    End Select
    Exit Function
EH:
    MC4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4500: MC5 = M4500()
    Case 4501: MC5 = M4501()
    Case 4502: MC5 = M4502()
    Case 4503: MC5 = M4503()
    Case 4504: MC5 = M4504()
    Case 4505: MC5 = M4505()
    Case 4506: MC5 = M4506()
    Case 4507: MC5 = M4507()
    Case 4508: MC5 = M4508()
    Case 4509: MC5 = M4509()
    Case 4510: MC5 = M4510()
    Case 4511: MC5 = M4511()
    Case 4512: MC5 = M4512()
    Case 4513: MC5 = M4513()
    Case 4514: MC5 = M4514()
    Case 4515: MC5 = M4515()
    Case 4516: MC5 = M4516()
    Case 4517: MC5 = M4517()
    Case 4518: MC5 = M4518()
    Case 4519: MC5 = M4519()
    Case 4520: MC5 = M4520()
    Case 4521: MC5 = M4521()
    Case 4522: MC5 = M4522()
    Case 4523: MC5 = M4523()
    Case 4524: MC5 = M4524()
    Case 4525: MC5 = M4525()
    Case 4526: MC5 = M4526()
    Case 4527: MC5 = M4527()
    Case 4528: MC5 = M4528()
    Case 4529: MC5 = M4529()
    Case 4530: MC5 = M4530()
    Case 4531: MC5 = M4531()
    Case 4532: MC5 = M4532()
    Case 4533: MC5 = M4533()
    Case 4534: MC5 = M4534()
    Case 4535: MC5 = M4535()
    Case 4536: MC5 = M4536()
    Case 4537: MC5 = M4537()
    Case 4538: MC5 = M4538()
    Case 4539: MC5 = M4539()
    Case 4540: MC5 = M4540()
    Case 4541: MC5 = M4541()
    Case 4542: MC5 = M4542()
    Case 4543: MC5 = M4543()
    Case 4544: MC5 = M4544()
    Case 4545: MC5 = M4545()
    Case 4546: MC5 = M4546()
    Case 4547: MC5 = M4547()
    Case 4548: MC5 = M4548()
    Case 4549: MC5 = M4549()
    Case 4550: MC5 = M4550()
    Case 4551: MC5 = M4551()
    Case 4552: MC5 = M4552()
    Case 4553: MC5 = M4553()
    Case 4554: MC5 = M4554()
    Case 4555: MC5 = M4555()
    Case 4556: MC5 = M4556()
    Case 4557: MC5 = M4557()
    Case 4558: MC5 = M4558()
    Case 4559: MC5 = M4559()
    Case 4560: MC5 = M4560()
    Case 4561: MC5 = M4561()
    Case 4562: MC5 = M4562()
    Case 4563: MC5 = M4563()
    Case 4564: MC5 = M4564()
    Case 4565: MC5 = M4565()
    Case 4566: MC5 = M4566()
    Case 4567: MC5 = M4567()
    Case 4568: MC5 = M4568()
    Case 4569: MC5 = M4569()
    Case 4570: MC5 = M4570()
    Case 4571: MC5 = M4571()
    Case 4572: MC5 = M4572()
    Case 4573: MC5 = M4573()
    Case 4574: MC5 = M4574()
    Case 4575: MC5 = M4575()
    Case 4576: MC5 = M4576()
    Case 4577: MC5 = M4577()
    Case 4578: MC5 = M4578()
    Case 4579: MC5 = M4579()
    Case 4580: MC5 = M4580()
    Case 4581: MC5 = M4581()
    Case 4582: MC5 = M4582()
    Case 4583: MC5 = M4583()
    Case 4584: MC5 = M4584()
    Case 4585: MC5 = M4585()
    Case 4586: MC5 = M4586()
    Case 4587: MC5 = M4587()
    Case 4588: MC5 = M4588()
    Case 4589: MC5 = M4589()
    Case 4590: MC5 = M4590()
    Case 4591: MC5 = M4591()
    Case 4592: MC5 = M4592()
    Case 4593: MC5 = M4593()
    Case 4594: MC5 = M4594()
    Case 4595: MC5 = M4595()
    Case 4596: MC5 = M4596()
    Case 4597: MC5 = M4597()
    Case 4598: MC5 = M4598()
    Case 4599: MC5 = M4599()
    End Select
    Exit Function
EH:
    MC5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC6(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4600: MC6 = M4600()
    Case 4601: MC6 = M4601()
    Case 4602: MC6 = M4602()
    Case 4603: MC6 = M4603()
    Case 4604: MC6 = M4604()
    Case 4605: MC6 = M4605()
    Case 4606: MC6 = M4606()
    Case 4607: MC6 = M4607()
    Case 4608: MC6 = M4608()
    Case 4609: MC6 = M4609()
    Case 4610: MC6 = M4610()
    Case 4611: MC6 = M4611()
    Case 4612: MC6 = M4612()
    Case 4613: MC6 = M4613()
    Case 4614: MC6 = M4614()
    Case 4615: MC6 = M4615()
    Case 4616: MC6 = M4616()
    Case 4617: MC6 = M4617()
    Case 4618: MC6 = M4618()
    Case 4619: MC6 = M4619()
    Case 4620: MC6 = M4620()
    Case 4621: MC6 = M4621()
    Case 4622: MC6 = M4622()
    Case 4623: MC6 = M4623()
    Case 4624: MC6 = M4624()
    Case 4625: MC6 = M4625()
    Case 4626: MC6 = M4626()
    Case 4627: MC6 = M4627()
    Case 4628: MC6 = M4628()
    Case 4629: MC6 = M4629()
    Case 4630: MC6 = M4630()
    Case 4631: MC6 = M4631()
    Case 4632: MC6 = M4632()
    Case 4633: MC6 = M4633()
    Case 4634: MC6 = M4634()
    Case 4635: MC6 = M4635()
    Case 4636: MC6 = M4636()
    Case 4637: MC6 = M4637()
    Case 4638: MC6 = M4638()
    Case 4639: MC6 = M4639()
    Case 4640: MC6 = M4640()
    Case 4641: MC6 = M4641()
    Case 4642: MC6 = M4642()
    Case 4643: MC6 = M4643()
    Case 4644: MC6 = M4644()
    Case 4645: MC6 = M4645()
    Case 4646: MC6 = M4646()
    Case 4647: MC6 = M4647()
    Case 4648: MC6 = M4648()
    Case 4649: MC6 = M4649()
    Case 4650: MC6 = M4650()
    Case 4651: MC6 = M4651()
    Case 4652: MC6 = M4652()
    Case 4653: MC6 = M4653()
    Case 4654: MC6 = M4654()
    Case 4655: MC6 = M4655()
    Case 4656: MC6 = M4656()
    Case 4657: MC6 = M4657()
    Case 4658: MC6 = M4658()
    Case 4659: MC6 = M4659()
    Case 4660: MC6 = M4660()
    Case 4661: MC6 = M4661()
    Case 4662: MC6 = M4662()
    Case 4663: MC6 = M4663()
    Case 4664: MC6 = M4664()
    Case 4665: MC6 = M4665()
    Case 4666: MC6 = M4666()
    Case 4667: MC6 = M4667()
    Case 4668: MC6 = M4668()
    Case 4669: MC6 = M4669()
    Case 4670: MC6 = M4670()
    Case 4671: MC6 = M4671()
    Case 4672: MC6 = M4672()
    Case 4673: MC6 = M4673()
    Case 4674: MC6 = M4674()
    Case 4675: MC6 = M4675()
    Case 4676: MC6 = M4676()
    Case 4677: MC6 = M4677()
    Case 4678: MC6 = M4678()
    Case 4679: MC6 = M4679()
    Case 4680: MC6 = M4680()
    Case 4681: MC6 = M4681()
    Case 4682: MC6 = M4682()
    Case 4683: MC6 = M4683()
    Case 4684: MC6 = M4684()
    Case 4685: MC6 = M4685()
    Case 4686: MC6 = M4686()
    Case 4687: MC6 = M4687()
    Case 4688: MC6 = M4688()
    Case 4689: MC6 = M4689()
    Case 4690: MC6 = M4690()
    Case 4691: MC6 = M4691()
    Case 4692: MC6 = M4692()
    Case 4693: MC6 = M4693()
    Case 4694: MC6 = M4694()
    Case 4695: MC6 = M4695()
    Case 4696: MC6 = M4696()
    Case 4697: MC6 = M4697()
    Case 4698: MC6 = M4698()
    Case 4699: MC6 = M4699()
    End Select
    Exit Function
EH:
    MC6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC7(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4700: MC7 = M4700()
    Case 4701: MC7 = M4701()
    Case 4702: MC7 = M4702()
    Case 4703: MC7 = M4703()
    Case 4704: MC7 = M4704()
    Case 4705: MC7 = M4705()
    Case 4706: MC7 = M4706()
    Case 4707: MC7 = M4707()
    Case 4708: MC7 = M4708()
    Case 4709: MC7 = M4709()
    Case 4710: MC7 = M4710()
    Case 4711: MC7 = M4711()
    Case 4712: MC7 = M4712()
    Case 4713: MC7 = M4713()
    Case 4714: MC7 = M4714()
    Case 4715: MC7 = M4715()
    Case 4716: MC7 = M4716()
    Case 4717: MC7 = M4717()
    Case 4718: MC7 = M4718()
    Case 4719: MC7 = M4719()
    Case 4720: MC7 = M4720()
    Case 4721: MC7 = M4721()
    Case 4722: MC7 = M4722()
    Case 4723: MC7 = M4723()
    Case 4724: MC7 = M4724()
    Case 4725: MC7 = M4725()
    Case 4726: MC7 = M4726()
    Case 4727: MC7 = M4727()
    Case 4728: MC7 = M4728()
    Case 4729: MC7 = M4729()
    Case 4730: MC7 = M4730()
    Case 4731: MC7 = M4731()
    Case 4732: MC7 = M4732()
    Case 4733: MC7 = M4733()
    Case 4734: MC7 = M4734()
    Case 4735: MC7 = M4735()
    Case 4736: MC7 = M4736()
    Case 4737: MC7 = M4737()
    Case 4738: MC7 = M4738()
    Case 4739: MC7 = M4739()
    Case 4740: MC7 = M4740()
    Case 4741: MC7 = M4741()
    Case 4742: MC7 = M4742()
    Case 4743: MC7 = M4743()
    Case 4744: MC7 = M4744()
    Case 4745: MC7 = M4745()
    Case 4746: MC7 = M4746()
    Case 4747: MC7 = M4747()
    Case 4748: MC7 = M4748()
    Case 4749: MC7 = M4749()
    Case 4750: MC7 = M4750()
    Case 4751: MC7 = M4751()
    Case 4752: MC7 = M4752()
    Case 4753: MC7 = M4753()
    Case 4754: MC7 = M4754()
    Case 4755: MC7 = M4755()
    Case 4756: MC7 = M4756()
    Case 4757: MC7 = M4757()
    Case 4758: MC7 = M4758()
    Case 4759: MC7 = M4759()
    Case 4760: MC7 = M4760()
    Case 4761: MC7 = M4761()
    Case 4762: MC7 = M4762()
    Case 4763: MC7 = M4763()
    Case 4764: MC7 = M4764()
    Case 4765: MC7 = M4765()
    Case 4766: MC7 = M4766()
    Case 4767: MC7 = M4767()
    Case 4768: MC7 = M4768()
    Case 4769: MC7 = M4769()
    Case 4770: MC7 = M4770()
    Case 4771: MC7 = M4771()
    Case 4772: MC7 = M4772()
    Case 4773: MC7 = M4773()
    Case 4774: MC7 = M4774()
    Case 4775: MC7 = M4775()
    Case 4776: MC7 = M4776()
    Case 4777: MC7 = M4777()
    Case 4778: MC7 = M4778()
    Case 4779: MC7 = M4779()
    Case 4780: MC7 = M4780()
    Case 4781: MC7 = M4781()
    Case 4782: MC7 = M4782()
    Case 4783: MC7 = M4783()
    Case 4784: MC7 = M4784()
    Case 4785: MC7 = M4785()
    Case 4786: MC7 = M4786()
    Case 4787: MC7 = M4787()
    Case 4788: MC7 = M4788()
    Case 4789: MC7 = M4789()
    Case 4790: MC7 = M4790()
    Case 4791: MC7 = M4791()
    Case 4792: MC7 = M4792()
    Case 4793: MC7 = M4793()
    Case 4794: MC7 = M4794()
    Case 4795: MC7 = M4795()
    Case 4796: MC7 = M4796()
    Case 4797: MC7 = M4797()
    Case 4798: MC7 = M4798()
    Case 4799: MC7 = M4799()
    End Select
    Exit Function
EH:
    MC7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC8(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4800: MC8 = M4800()
    Case 4801: MC8 = M4801()
    Case 4802: MC8 = M4802()
    Case 4803: MC8 = M4803()
    Case 4804: MC8 = M4804()
    Case 4805: MC8 = M4805()
    Case 4806: MC8 = M4806()
    Case 4807: MC8 = M4807()
    Case 4808: MC8 = M4808()
    Case 4809: MC8 = M4809()
    Case 4810: MC8 = M4810()
    Case 4811: MC8 = M4811()
    Case 4812: MC8 = M4812()
    Case 4813: MC8 = M4813()
    Case 4814: MC8 = M4814()
    Case 4815: MC8 = M4815()
    Case 4816: MC8 = M4816()
    Case 4817: MC8 = M4817()
    Case 4818: MC8 = M4818()
    Case 4819: MC8 = M4819()
    Case 4820: MC8 = M4820()
    Case 4821: MC8 = M4821()
    Case 4822: MC8 = M4822()
    Case 4823: MC8 = M4823()
    Case 4824: MC8 = M4824()
    Case 4825: MC8 = M4825()
    Case 4826: MC8 = M4826()
    Case 4827: MC8 = M4827()
    Case 4828: MC8 = M4828()
    Case 4829: MC8 = M4829()
    Case 4830: MC8 = M4830()
    Case 4831: MC8 = M4831()
    Case 4832: MC8 = M4832()
    Case 4833: MC8 = M4833()
    Case 4834: MC8 = M4834()
    Case 4835: MC8 = M4835()
    Case 4836: MC8 = M4836()
    Case 4837: MC8 = M4837()
    Case 4838: MC8 = M4838()
    Case 4839: MC8 = M4839()
    Case 4840: MC8 = M4840()
    Case 4841: MC8 = M4841()
    Case 4842: MC8 = M4842()
    Case 4843: MC8 = M4843()
    Case 4844: MC8 = M4844()
    Case 4845: MC8 = M4845()
    Case 4846: MC8 = M4846()
    Case 4847: MC8 = M4847()
    Case 4848: MC8 = M4848()
    Case 4849: MC8 = M4849()
    Case 4850: MC8 = M4850()
    Case 4851: MC8 = M4851()
    Case 4852: MC8 = M4852()
    Case 4853: MC8 = M4853()
    Case 4854: MC8 = M4854()
    Case 4855: MC8 = M4855()
    Case 4856: MC8 = M4856()
    Case 4857: MC8 = M4857()
    Case 4858: MC8 = M4858()
    Case 4859: MC8 = M4859()
    Case 4860: MC8 = M4860()
    Case 4861: MC8 = M4861()
    Case 4862: MC8 = M4862()
    Case 4863: MC8 = M4863()
    Case 4864: MC8 = M4864()
    Case 4865: MC8 = M4865()
    Case 4866: MC8 = M4866()
    Case 4867: MC8 = M4867()
    Case 4868: MC8 = M4868()
    Case 4869: MC8 = M4869()
    Case 4870: MC8 = M4870()
    Case 4871: MC8 = M4871()
    Case 4872: MC8 = M4872()
    Case 4873: MC8 = M4873()
    Case 4874: MC8 = M4874()
    Case 4875: MC8 = M4875()
    Case 4876: MC8 = M4876()
    Case 4877: MC8 = M4877()
    Case 4878: MC8 = M4878()
    Case 4879: MC8 = M4879()
    Case 4880: MC8 = M4880()
    Case 4881: MC8 = M4881()
    Case 4882: MC8 = M4882()
    Case 4883: MC8 = M4883()
    Case 4884: MC8 = M4884()
    Case 4885: MC8 = M4885()
    Case 4886: MC8 = M4886()
    Case 4887: MC8 = M4887()
    Case 4888: MC8 = M4888()
    Case 4889: MC8 = M4889()
    Case 4890: MC8 = M4890()
    Case 4891: MC8 = M4891()
    Case 4892: MC8 = M4892()
    Case 4893: MC8 = M4893()
    Case 4894: MC8 = M4894()
    Case 4895: MC8 = M4895()
    Case 4896: MC8 = M4896()
    Case 4897: MC8 = M4897()
    Case 4898: MC8 = M4898()
    Case 4899: MC8 = M4899()
    End Select
    Exit Function
EH:
    MC8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC9(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 4900: MC9 = M4900()
    Case 4901: MC9 = M4901()
    Case 4902: MC9 = M4902()
    Case 4903: MC9 = M4903()
    Case 4904: MC9 = M4904()
    Case 4905: MC9 = M4905()
    Case 4906: MC9 = M4906()
    Case 4907: MC9 = M4907()
    Case 4908: MC9 = M4908()
    Case 4909: MC9 = M4909()
    Case 4910: MC9 = M4910()
    Case 4911: MC9 = M4911()
    Case 4912: MC9 = M4912()
    Case 4913: MC9 = M4913()
    Case 4914: MC9 = M4914()
    Case 4915: MC9 = M4915()
    Case 4916: MC9 = M4916()
    Case 4917: MC9 = M4917()
    Case 4918: MC9 = M4918()
    Case 4919: MC9 = M4919()
    Case 4920: MC9 = M4920()
    Case 4921: MC9 = M4921()
    Case 4922: MC9 = M4922()
    Case 4923: MC9 = M4923()
    Case 4924: MC9 = M4924()
    Case 4925: MC9 = M4925()
    Case 4926: MC9 = M4926()
    Case 4927: MC9 = M4927()
    Case 4928: MC9 = M4928()
    Case 4929: MC9 = M4929()
    Case 4930: MC9 = M4930()
    Case 4931: MC9 = M4931()
    Case 4932: MC9 = M4932()
    Case 4933: MC9 = M4933()
    Case 4934: MC9 = M4934()
    Case 4935: MC9 = M4935()
    Case 4936: MC9 = M4936()
    Case 4937: MC9 = M4937()
    Case 4938: MC9 = M4938()
    Case 4939: MC9 = M4939()
    Case 4940: MC9 = M4940()
    Case 4941: MC9 = M4941()
    Case 4942: MC9 = M4942()
    Case 4943: MC9 = M4943()
    Case 4944: MC9 = M4944()
    Case 4945: MC9 = M4945()
    Case 4946: MC9 = M4946()
    Case 4947: MC9 = M4947()
    Case 4948: MC9 = M4948()
    Case 4949: MC9 = M4949()
    Case 4950: MC9 = M4950()
    Case 4951: MC9 = M4951()
    Case 4952: MC9 = M4952()
    Case 4953: MC9 = M4953()
    Case 4954: MC9 = M4954()
    Case 4955: MC9 = M4955()
    Case 4956: MC9 = M4956()
    Case 4957: MC9 = M4957()
    Case 4958: MC9 = M4958()
    Case 4959: MC9 = M4959()
    Case 4960: MC9 = M4960()
    Case 4961: MC9 = M4961()
    Case 4962: MC9 = M4962()
    Case 4963: MC9 = M4963()
    Case 4964: MC9 = M4964()
    Case 4965: MC9 = M4965()
    Case 4966: MC9 = M4966()
    Case 4967: MC9 = M4967()
    Case 4968: MC9 = M4968()
    Case 4969: MC9 = M4969()
    Case 4970: MC9 = M4970()
    Case 4971: MC9 = M4971()
    Case 4972: MC9 = M4972()
    Case 4973: MC9 = M4973()
    Case 4974: MC9 = M4974()
    Case 4975: MC9 = M4975()
    Case 4976: MC9 = M4976()
    Case 4977: MC9 = M4977()
    Case 4978: MC9 = M4978()
    Case 4979: MC9 = M4979()
    Case 4980: MC9 = M4980()
    Case 4981: MC9 = M4981()
    Case 4982: MC9 = M4982()
    Case 4983: MC9 = M4983()
    Case 4984: MC9 = M4984()
    Case 4985: MC9 = M4985()
    Case 4986: MC9 = M4986()
    Case 4987: MC9 = M4987()
    Case 4988: MC9 = M4988()
    Case 4989: MC9 = M4989()
    Case 4990: MC9 = M4990()
    Case 4991: MC9 = M4991()
    Case 4992: MC9 = M4992()
    Case 4993: MC9 = M4993()
    Case 4994: MC9 = M4994()
    Case 4995: MC9 = M4995()
    Case 4996: MC9 = M4996()
    Case 4997: MC9 = M4997()
    Case 4998: MC9 = M4998()
    Case 4999: MC9 = M4999()
    End Select
    Exit Function
EH:
    MC9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4000() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 1, 99999)
    M4000 = R
    Exit Function
EH:
    M4000 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4001() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 1, 0)
    M4001 = R
    Exit Function
EH:
    M4001 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4002() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 1, 1033) & StrConv("i", 1, 1055) & StrConv("I", 2, 1055)
    M4002 = R
    Exit Function
EH:
    M4002 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4003() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Space(0) & "|" & Space(3) & "|" & Len(Space(65536))
    M4003 = R
    Exit Function
EH:
    M4003 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4004() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Space(-1)
    M4004 = R
    Exit Function
EH:
    M4004 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4005() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Space(Null)
    M4005 = R
    Exit Function
EH:
    M4005 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4006() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = String(0, "a") & "|" & String(3, "ab") & "|" & String(3, 65) & "|" & String(3, 256) & "|" & String(2, "")
    M4006 = R
    Exit Function
EH:
    M4006 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4007() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = String(-1, "a")
    M4007 = R
    Exit Function
EH:
    M4007 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4008() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = String(2, 65536)
    M4008 = R
    Exit Function
EH:
    M4008 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4009() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = String(2, -1)
    M4009 = R
    Exit Function
EH:
    M4009 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4010() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = String(2, 0) = ChrW(0) & ChrW(0)
    M4010 = R
    Exit Function
EH:
    M4010 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4011() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = String(2, 65535) = ChrW(65535) & ChrW(65535)
    M4011 = R
    Exit Function
EH:
    M4011 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4012() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Chr(65) & Chr(0) & Chr(255) & "|" & Asc(Chr(255))
    M4012 = R
    Exit Function
EH:
    M4012 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4013() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Chr(256)
    M4013 = R
    Exit Function
EH:
    M4013 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4014() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Chr(-1)
    M4014 = R
    Exit Function
EH:
    M4014 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4015() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Chr(Null)
    M4015 = R
    Exit Function
EH:
    M4015 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4016() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = ChrW(65536)
    M4016 = R
    Exit Function
EH:
    M4016 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4017() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = ChrW(-1) = ChrW(65535)
    M4017 = R
    Exit Function
EH:
    M4017 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4018() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = ChrW(-32768) = ChrW(32768)
    M4018 = R
    Exit Function
EH:
    M4018 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4019() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = ChrW(-32769)
    M4019 = R
    Exit Function
EH:
    M4019 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4020() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Chr$(65.5) & Chr$(66.5) & Chr$(67.5)
    M4020 = R
    Exit Function
EH:
    M4020 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4021() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Asc("")
    M4021 = R
    Exit Function
EH:
    M4021 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4022() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = AscW("")
    M4022 = R
    Exit Function
EH:
    M4022 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4023() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Asc(Null)
    M4023 = R
    Exit Function
EH:
    M4023 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4024() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Asc("abc") & AscW("abc") & AscW(ChrW(65535)) & AscW(ChrW(40000)) & "|" & Asc(ChrW(8364)) & "|" & Asc(ChrW(955)) & "|" & Asc(ChrW(&H2018))
    M4024 = R
    Exit Function
EH:
    M4024 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4025() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Len(ChrW(&HD83D) & ChrW(&HDE00)) & Len(UCase(ChrW(&HD83D) & ChrW(&HDE00))) & (StrReverse(ChrW(&HD83D) & ChrW(&HDE00) & "a") = "a" & ChrW(&HDE00) & ChrW(&HD83D))
    M4025 = R
    Exit Function
EH:
    M4025 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4026() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrReverse("") & "|" & StrReverse("a") & "|" & StrReverse("abc") & "|" & StrReverse(123) & "|" & TypeName(StrReverse(123))
    M4026 = R
    Exit Function
EH:
    M4026 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4027() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrReverse(Null)
    M4027 = R
    Exit Function
EH:
    M4027 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4028() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(StrReverse(Empty)))
    M4028 = R
    Exit Function
EH:
    M4028 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4029() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = LCase(ChrW(&H130)) = "i" & ChrW(&H307)
    M4029 = R
    Exit Function
EH:
    M4029 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4030() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Len(LCase(ChrW(&H130)))
    M4030 = R
    Exit Function
EH:
    M4030 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4031() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = AscW(UCase(ChrW(&HDF))) & "|" & Len(UCase(ChrW(&HDF))) & "|" & AscW(UCase(ChrW(&H131))) & "|" & AscW(UCase(ChrW(&HFF))) & "|" & AscW(LCase(ChrW(&H3A3)))
    M4031 = R
    Exit Function
EH:
    M4031 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4032() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Len(Trim(ChrW(160) & "a" & ChrW(160))) & Len(Trim(vbTab & "a" & vbTab)) & Len(Trim(ChrW(12288) & "a")) & Len(Trim(vbCr & vbLf & "a"))
    M4032 = R
    Exit Function
EH:
    M4032 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4033() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = LTrim("  a ") & "|" & RTrim("  a ") & "|" & Trim(5) & "|" & TypeName(Trim(5)) & "|" & TypeName(Trim$("5")) & "|" & TypeName(LTrim(Empty))
    M4033 = R
    Exit Function
EH:
    M4033 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4034() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Left("abc", 5) & Right("abc", 5) & Mid("abc", 2, 100) & Left("abc", 1.5) & Left("abc", 2.5) & Left("abc", 0.5) & Right("abc", 1.5)
    M4034 = R
    Exit Function
EH:
    M4034 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4035() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Left("abc", Null)
    M4035 = R
    Exit Function
EH:
    M4035 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4036() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Left("abc", "x")
    M4036 = R
    Exit Function
EH:
    M4036 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4037() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Left(123, 2) & TypeName(Left(123, 2))
    M4037 = R
    Exit Function
EH:
    M4037 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4038() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Mid("abc", 2.5) & "|" & Mid("abcdef", 1.5, 1.5) & "|" & Mid("abc", 4) & "|" & Mid("abc", 4, 1) & "|" & Mid("abc", 3, 0)
    M4038 = R
    Exit Function
EH:
    M4038 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4039() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Mid("abc", 0, 1)
    M4039 = R
    Exit Function
EH:
    M4039 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4040() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Mid("abc", -1)
    M4040 = R
    Exit Function
EH:
    M4040 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4041() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Mid("abc", 2, -1)
    M4041 = R
    Exit Function
EH:
    M4041 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4042() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Mid("abc", 2, Null)
    M4042 = R
    Exit Function
EH:
    M4042 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4043() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Mid("abc", Null)
    M4043 = R
    Exit Function
EH:
    M4043 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4044() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abcdef"
    Mid(s, 2, 2) = "XYZ"
    R = s
    M4044 = R
    Exit Function
EH:
    M4044 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4045() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abcdef"
    Mid(s, 2) = "XY"
    R = s
    M4045 = R
    Exit Function
EH:
    M4045 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4046() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 2, 5) = "XYZW"
    R = s
    M4046 = R
    Exit Function
EH:
    M4046 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4047() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 4) = "X"
    R = s
    M4047 = R
    Exit Function
EH:
    M4047 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4048() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 0) = "X"
    R = s
    M4048 = R
    Exit Function
EH:
    M4048 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4049() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 1, 0) = "X"
    R = s
    M4049 = R
    Exit Function
EH:
    M4049 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4050() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 1) = ""
    R = s
    M4050 = R
    Exit Function
EH:
    M4050 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4051() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Variant
    s = "abc"
    Mid(s, 2) = "Z"
    R = s
    M4051 = R
    Exit Function
EH:
    M4051 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4052() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Variant
    s = 12345
    Mid(s, 2) = "Z"
    R = Dump(s)
    M4052 = R
    Exit Function
EH:
    M4052 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4053() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As Variant
    s = Null
    Mid(s, 2) = "Z"
    R = Dump(s)
    M4053 = R
    Exit Function
EH:
    M4053 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4054() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 2) = Null
    R = s
    M4054 = R
    Exit Function
EH:
    M4054 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4055() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    Mid(s, 2, 1) = 5
    R = s
    M4055 = R
    Exit Function
EH:
    M4055 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4056() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 5
    s = "ab"
    R = "[" & s & "]" & Len(s) & LenB(s)
    M4056 = R
    Exit Function
EH:
    M4056 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4057() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 3
    s = "abcdef"
    R = "[" & s & "]"
    M4057 = R
    Exit Function
EH:
    M4057 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4058() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 3
    R = "[" & s & "]" & Len(s) & (s = String(3, 0))
    M4058 = R
    Exit Function
EH:
    M4058 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4059() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 5
    LSet s = "ab"
    R = "[" & s & "]"
    M4059 = R
    Exit Function
EH:
    M4059 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4060() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 5
    RSet s = "ab"
    R = "[" & s & "]"
    M4060 = R
    Exit Function
EH:
    M4060 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4061() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 3
    RSet s = "abcdef"
    R = "[" & s & "]"
    M4061 = R
    Exit Function
EH:
    M4061 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4062() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 3
    LSet s = "abcdef"
    R = "[" & s & "]"
    M4062 = R
    Exit Function
EH:
    M4062 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4063() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "xxxxx"
    RSet s = "ab"
    R = "[" & s & "]"
    M4063 = R
    Exit Function
EH:
    M4063 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4064() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "xxxxx"
    LSet s = "ab"
    R = "[" & s & "]"
    M4064 = R
    Exit Function
EH:
    M4064 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4065() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 4
    s = "ab"
    R = "[" & Trim(s) & "]" & Len(Trim(s)) & Len(s & "x") & TypeName(s) & VarType(s)
    M4065 = R
    Exit Function
EH:
    M4065 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4066() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 2
    Dim v As Variant
    v = s
    R = Dump(v)
    M4066 = R
    Exit Function
EH:
    M4066 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4067() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 3
    s = ChrW(233)
    R = Len(s) & LenB(s) & AscW(s)
    M4067 = R
    Exit Function
EH:
    M4067 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4068() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 4
    s = 12
    R = "[" & s & "]"
    M4068 = R
    Exit Function
EH:
    M4068 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4069() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 4
    s = Null
    R = "[" & s & "]"
    M4069 = R
    Exit Function
EH:
    M4069 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4070() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String * 4
    s = Empty
    R = "[" & s & "]" & Len(s)
    M4070 = R
    Exit Function
EH:
    M4070 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4071() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like "a")) & Dump(fV("a" Like "A")) & Dump(fV("abc" Like "a*c")) & Dump(fV("ac" Like "a*c")) & Dump(fV("a" Like "?")) & Dump(fV("" Like "?")) & Dump(fV("" Like "*")) & Dump(fV("" Like ""))
    M4071 = R
    Exit Function
EH:
    M4071 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4072() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" Like "#")) & Dump(fV("a" Like "#")) & Dump(fV("a-b" Like "a[-]b")) & Dump(fV("a-b" Like "a[+-]b")) & Dump(fV("]" Like "[]]")) & Dump(fV("a" Like "[!b-z]")) & Dump(fV("^" Like "[^a]")) & Dump(fV("a" Like "[^a]"))
    M4072 = R
    Exit Function
EH:
    M4072 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4073() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("*" Like "[*]")) & Dump(fV("?" Like "[?]")) & Dump(fV("#" Like "[#]")) & Dump(fV("[" Like "[[]")) & Dump(fV("ab" Like "[a-b][a-b]")) & Dump(fV("abc" Like "a[a-z]c")) & Dump(fV("aBc" Like "a[a-z]c")) & Dump(fV("B" Like "[a-z]"))
    M4073 = R
    Exit Function
EH:
    M4073 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4074() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" Like "a**c")) & Dump(fV("abc" Like "*?*")) & Dump(fV("abc" Like "???")) & Dump(fV("abc" Like "??")) & Dump(fV("ab" Like "a*b*")) & Dump(fV("ab" Like "*b*a")) & Dump(fV("aXb" Like "a?b"))
    M4074 = R
    Exit Function
EH:
    M4074 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4075() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" & ChrW(233) & "b" Like "a[" & ChrW(224) & "-" & ChrW(255) & "]b")) & Dump(fV("A" Like "[a-z]")) & Dump(fV(ChrW(233) Like "e")) & Dump(fV(ChrW(201) Like ChrW(233)))
    M4075 = R
    Exit Function
EH:
    M4075 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4076() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like "[a-a]")) & Dump(fV("b" Like "[a-a]")) & Dump(fV("-" Like "[a-]")) & Dump(fV("-" Like "[-a]")) & Dump(fV("b" Like "[a-]")) & Dump(fV("!" Like "[!]")) & Dump(fV("a" Like "[!]"))
    M4076 = R
    Exit Function
EH:
    M4076 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4077() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("1" Like "[0-9]")) & Dump(fV("12" Like "##")) & Dump(fV("1a" Like "#[a-z]")) & Dump(fV(ChrW(1633) Like "#"))
    M4077 = R
    Exit Function
EH:
    M4077 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4078() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null Like "a"))
    M4078 = R
    Exit Function
EH:
    M4078 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4079() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like Null))
    M4079 = R
    Exit Function
EH:
    M4079 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4080() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Like "#"))
    M4080 = R
    Exit Function
EH:
    M4080 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4081() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty Like ""))
    M4081 = R
    Exit Function
EH:
    M4081 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4082() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like 5))
    M4082 = R
    Exit Function
EH:
    M4082 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4083() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" Like "a["))
    M4083 = R
    Exit Function
EH:
    M4083 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4084() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" Like "a[]c"))
    M4084 = R
    Exit Function
EH:
    M4084 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4085() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like "[a"))
    M4085 = R
    Exit Function
EH:
    M4085 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4086() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like "[!a"))
    M4086 = R
    Exit Function
EH:
    M4086 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4087() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" Like "*[]"))
    M4087 = R
    Exit Function
EH:
    M4087 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4088() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" Like "[z-a]"))
    M4088 = R
    Exit Function
EH:
    M4088 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4089() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" Like "ABC")) & Dump(fV(ChrW(192) & "B" Like ChrW(224) & "b"))
    M4089 = R
    Exit Function
EH:
    M4089 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4090() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 + "1"))
    M4090 = R
    Exit Function
EH:
    M4090 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4091() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("1" + "1"))
    M4091 = R
    Exit Function
EH:
    M4091 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4092() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("1" & 1))
    M4092 = R
    Exit Function
EH:
    M4092 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4093() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" + 1))
    M4093 = R
    Exit Function
EH:
    M4093 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4094() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("1" + Null))
    M4094 = R
    Exit Function
EH:
    M4094 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4095() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null + 1))
    M4095 = R
    Exit Function
EH:
    M4095 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4096() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Null & "a"))
    M4096 = R
    Exit Function
EH:
    M4096 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4097() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty + 1))
    M4097 = R
    Exit Function
EH:
    M4097 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4098() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty & "a"))
    M4098 = R
    Exit Function
EH:
    M4098 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4099() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty + Empty))
    M4099 = R
    Exit Function
EH:
    M4099 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4100() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty & Empty))
    M4100 = R
    Exit Function
EH:
    M4100 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4101() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True + True))
    M4101 = R
    Exit Function
EH:
    M4101 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4102() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True & True))
    M4102 = R
    Exit Function
EH:
    M4102 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4103() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-True))
    M4103 = R
    Exit Function
EH:
    M4103 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4104() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" * "5"))
    M4104 = R
    Exit Function
EH:
    M4104 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4105() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" - "2"))
    M4105 = R
    Exit Function
EH:
    M4105 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4106() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" / "2"))
    M4106 = R
    Exit Function
EH:
    M4106 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4107() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" \ "2"))
    M4107 = R
    Exit Function
EH:
    M4107 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4108() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" Mod "3"))
    M4108 = R
    Exit Function
EH:
    M4108 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4109() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" ^ "2"))
    M4109 = R
    Exit Function
EH:
    M4109 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4110() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 / 0))
    M4110 = R
    Exit Function
EH:
    M4110 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4111() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 \ 0))
    M4111 = R
    Exit Function
EH:
    M4111 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4112() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 Mod 0))
    M4112 = R
    Exit Function
EH:
    M4112 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4113() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 / 0))
    M4113 = R
    Exit Function
EH:
    M4113 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4114() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 ^ -1))
    M4114 = R
    Exit Function
EH:
    M4114 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4115() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0 ^ 0))
    M4115 = R
    Exit Function
EH:
    M4115 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4116() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV((-8) ^ (1 / 3)))
    M4116 = R
    Exit Function
EH:
    M4116 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4117() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV((-8) ^ 2))
    M4117 = R
    Exit Function
EH:
    M4117 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4118() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-8 ^ 2))
    M4118 = R
    Exit Function
EH:
    M4118 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4119() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2 ^ 3 ^ 2))
    M4119 = R
    Exit Function
EH:
    M4119 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4120() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2 ^ -1))
    M4120 = R
    Exit Function
EH:
    M4120 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4121() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-2 ^ -2))
    M4121 = R
    Exit Function
EH:
    M4121 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4122() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10 \ 3))
    M4122 = R
    Exit Function
EH:
    M4122 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4123() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-10 \ 3))
    M4123 = R
    Exit Function
EH:
    M4123 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4124() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10 \ -3))
    M4124 = R
    Exit Function
EH:
    M4124 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4125() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10 Mod 3))
    M4125 = R
    Exit Function
EH:
    M4125 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4126() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-10 Mod 3))
    M4126 = R
    Exit Function
EH:
    M4126 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4127() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10 Mod -3))
    M4127 = R
    Exit Function
EH:
    M4127 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4128() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10.5 Mod 3))
    M4128 = R
    Exit Function
EH:
    M4128 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4129() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10.5 \ 3))
    M4129 = R
    Exit Function
EH:
    M4129 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4130() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2.5 \ 1))
    M4130 = R
    Exit Function
EH:
    M4130 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4131() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(3.5 \ 1))
    M4131 = R
    Exit Function
EH:
    M4131 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4132() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-2.5 \ 1))
    M4132 = R
    Exit Function
EH:
    M4132 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4133() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Mod 2.5))
    M4133 = R
    Exit Function
EH:
    M4133 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4134() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Mod 2.4))
    M4134 = R
    Exit Function
EH:
    M4134 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4135() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Mod 0.4))
    M4135 = R
    Exit Function
EH:
    M4135 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4136() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2147483647 + 1))
    M4136 = R
    Exit Function
EH:
    M4136 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4137() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2147483647& + 1))
    M4137 = R
    Exit Function
EH:
    M4137 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4138() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(32767 + 1))
    M4138 = R
    Exit Function
EH:
    M4138 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4139() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(32767% + 1%))
    M4139 = R
    Exit Function
EH:
    M4139 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4140() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-32768 - 1))
    M4140 = R
    Exit Function
EH:
    M4140 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4141() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(255 + 1))
    M4141 = R
    Exit Function
EH:
    M4141 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4142() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(255) + CByte(1)))
    M4142 = R
    Exit Function
EH:
    M4142 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4143() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(255) + 1))
    M4143 = R
    Exit Function
EH:
    M4143 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4144() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(0) - 1))
    M4144 = R
    Exit Function
EH:
    M4144 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4145() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(32767) * 2))
    M4145 = R
    Exit Function
EH:
    M4145 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4146() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CLng(65536) * 65536))
    M4146 = R
    Exit Function
EH:
    M4146 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4147() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(65536 * 65536))
    M4147 = R
    Exit Function
EH:
    M4147 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4148() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(65536& * 65536&))
    M4148 = R
    Exit Function
EH:
    M4148 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4149() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(100000 * 100000))
    M4149 = R
    Exit Function
EH:
    M4149 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4150() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(3 / 2))
    M4150 = R
    Exit Function
EH:
    M4150 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4151() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(4 / 2))
    M4151 = R
    Exit Function
EH:
    M4151 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4152() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(3) / CInt(2)))
    M4152 = R
    Exit Function
EH:
    M4152 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4153() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CInt(4) / CInt(2))))
    M4153 = R
    Exit Function
EH:
    M4153 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4154() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CLng(4) \ CLng(2))))
    M4154 = R
    Exit Function
EH:
    M4154 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4155() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CInt(4) \ 2)))
    M4155 = R
    Exit Function
EH:
    M4155 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4156() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CByte(4) + CByte(2))))
    M4156 = R
    Exit Function
EH:
    M4156 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4157() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CByte(4) * 2)))
    M4157 = R
    Exit Function
EH:
    M4157 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4158() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CInt(4) * CLng(2))))
    M4158 = R
    Exit Function
EH:
    M4158 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4159() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CSng(4) * 2)))
    M4159 = R
    Exit Function
EH:
    M4159 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4160() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CSng(4) * CDbl(2))))
    M4160 = R
    Exit Function
EH:
    M4160 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4161() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CCur(4) * 2)))
    M4161 = R
    Exit Function
EH:
    M4161 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4162() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CCur(4) * 2.5)))
    M4162 = R
    Exit Function
EH:
    M4162 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4163() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CDec(4) * 2)))
    M4163 = R
    Exit Function
EH:
    M4163 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4164() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CDec(4) * 2.5)))
    M4164 = R
    Exit Function
EH:
    M4164 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4165() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CDec(4) / 2)))
    M4165 = R
    Exit Function
EH:
    M4165 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4166() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(True + 1)))
    M4166 = R
    Exit Function
EH:
    M4166 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4167() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(True + True)))
    M4167 = R
    Exit Function
EH:
    M4167 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4168() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(True * True)))
    M4168 = R
    Exit Function
EH:
    M4168 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4169() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(True And True)))
    M4169 = R
    Exit Function
EH:
    M4169 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4170() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(1 And True)))
    M4170 = R
    Exit Function
EH:
    M4170 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4171() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(#1/1/2000# + 1)))
    M4171 = R
    Exit Function
EH:
    M4171 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4172() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(#1/1/2000# - #1/1/2000#)))
    M4172 = R
    Exit Function
EH:
    M4172 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4173() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName("1" + 1)))
    M4173 = R
    Exit Function
EH:
    M4173 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4174() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName("1" + "1")))
    M4174 = R
    Exit Function
EH:
    M4174 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4175() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName("1.5" + "1")))
    M4175 = R
    Exit Function
EH:
    M4175 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4176() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Empty + Empty)))
    M4176 = R
    Exit Function
EH:
    M4176 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4177() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Empty * 1)))
    M4177 = R
    Exit Function
EH:
    M4177 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4178() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null + 1)))
    M4178 = R
    Exit Function
EH:
    M4178 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4179() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(1 + Null)))
    M4179 = R
    Exit Function
EH:
    M4179 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4180() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Empty & Empty)))
    M4180 = R
    Exit Function
EH:
    M4180 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4181() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null & Null)))
    M4181 = R
    Exit Function
EH:
    M4181 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4182() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null And 1)))
    M4182 = R
    Exit Function
EH:
    M4182 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4183() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null And 0)))
    M4183 = R
    Exit Function
EH:
    M4183 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4184() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null Or 1)))
    M4184 = R
    Exit Function
EH:
    M4184 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4185() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null Or 0)))
    M4185 = R
    Exit Function
EH:
    M4185 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4186() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null Xor 1)))
    M4186 = R
    Exit Function
EH:
    M4186 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4187() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null Eqv 1)))
    M4187 = R
    Exit Function
EH:
    M4187 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4188() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null Imp 1)))
    M4188 = R
    Exit Function
EH:
    M4188 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4189() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null Imp 0)))
    M4189 = R
    Exit Function
EH:
    M4189 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4190() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(0 Imp Null)))
    M4190 = R
    Exit Function
EH:
    M4190 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4191() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(1 Imp Null)))
    M4191 = R
    Exit Function
EH:
    M4191 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4192() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null = 1)))
    M4192 = R
    Exit Function
EH:
    M4192 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4193() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Null = Null)))
    M4193 = R
    Exit Function
EH:
    M4193 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4194() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Not Null)))
    M4194 = R
    Exit Function
EH:
    M4194 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4195() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(-Null)))
    M4195 = R
    Exit Function
EH:
    M4195 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4196() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Empty = 0)))
    M4196 = R
    Exit Function
EH:
    M4196 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4197() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Empty = "")))
    M4197 = R
    Exit Function
EH:
    M4197 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4198() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Empty = Empty)))
    M4198 = R
    Exit Function
EH:
    M4198 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4199() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV((Empty = 0) & (Empty = "") & (0 = "")))
    M4199 = R
    Exit Function
EH:
    M4199 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4200() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("10" < "9"))
    M4200 = R
    Exit Function
EH:
    M4200 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4201() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("10" < 9))
    M4201 = R
    Exit Function
EH:
    M4201 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4202() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(10 < "9"))
    M4202 = R
    Exit Function
EH:
    M4202 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4203() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" < 1))
    M4203 = R
    Exit Function
EH:
    M4203 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4204() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" = 1))
    M4204 = R
    Exit Function
EH:
    M4204 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4205() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty < 1))
    M4205 = R
    Exit Function
EH:
    M4205 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4206() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Empty < "a"))
    M4206 = R
    Exit Function
EH:
    M4206 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4207() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("" < Empty))
    M4207 = R
    Exit Function
EH:
    M4207 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4208() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# = 36526))
    M4208 = R
    Exit Function
EH:
    M4208 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4209() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(#1/1/2000# = "1/1/2000"))
    M4209 = R
    Exit Function
EH:
    M4209 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4210() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1 = 1.0000000000000002))
    M4210 = R
    Exit Function
EH:
    M4210 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4211() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(0.1) = 0.1))
    M4211 = R
    Exit Function
EH:
    M4211 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4212() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(0.1) = 0.1))
    M4212 = R
    Exit Function
EH:
    M4212 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4213() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(0.1) = 0.1))
    M4213 = R
    Exit Function
EH:
    M4213 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4214() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1) / 3 * 3))
    M4214 = R
    Exit Function
EH:
    M4214 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4215() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(1) / 3 * 3))
    M4215 = R
    Exit Function
EH:
    M4215 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4216() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("abc" < "abd"))
    M4216 = R
    Exit Function
EH:
    M4216 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4217() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("B" < "a"))
    M4217 = R
    Exit Function
EH:
    M4217 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4218() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" < "B"))
    M4218 = R
    Exit Function
EH:
    M4218 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4219() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True = -1))
    M4219 = R
    Exit Function
EH:
    M4219 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4220() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(True = "True"))
    M4220 = R
    Exit Function
EH:
    M4220 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4221() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(True) = "True"))
    M4221 = R
    Exit Function
EH:
    M4221 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4222() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("True" = True))
    M4222 = R
    Exit Function
EH:
    M4222 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4223() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 And 3))
    M4223 = R
    Exit Function
EH:
    M4223 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4224() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Or 3))
    M4224 = R
    Exit Function
EH:
    M4224 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4225() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Xor 3))
    M4225 = R
    Exit Function
EH:
    M4225 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4226() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Eqv 3))
    M4226 = R
    Exit Function
EH:
    M4226 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4227() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5 Imp 3))
    M4227 = R
    Exit Function
EH:
    M4227 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4228() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 5))
    M4228 = R
    Exit Function
EH:
    M4228 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4229() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not -1))
    M4229 = R
    Exit Function
EH:
    M4229 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4230() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not 0))
    M4230 = R
    Exit Function
EH:
    M4230 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4231() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5.5 And 3))
    M4231 = R
    Exit Function
EH:
    M4231 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4232() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2.5 And 3))
    M4232 = R
    Exit Function
EH:
    M4232 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4233() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(3.5 And 3))
    M4233 = R
    Exit Function
EH:
    M4233 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4234() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("5" And 3))
    M4234 = R
    Exit Function
EH:
    M4234 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4235() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV("a" And 3))
    M4235 = R
    Exit Function
EH:
    M4235 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4236() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E10 And 3))
    M4236 = R
    Exit Function
EH:
    M4236 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4237() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-1E10 Or 1))
    M4237 = R
    Exit Function
EH:
    M4237 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4238() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2147483648 And 1))
    M4238 = R
    Exit Function
EH:
    M4238 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4239() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(2147483647 And 1))
    M4239 = R
    Exit Function
EH:
    M4239 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4240() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&HFFFF))
    M4240 = R
    Exit Function
EH:
    M4240 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4241() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&HFFFF&))
    M4241 = R
    Exit Function
EH:
    M4241 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4242() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&H8000))
    M4242 = R
    Exit Function
EH:
    M4242 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4243() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&H7FFF))
    M4243 = R
    Exit Function
EH:
    M4243 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4244() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&H10000))
    M4244 = R
    Exit Function
EH:
    M4244 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4245() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&HFFFFFFFF))
    M4245 = R
    Exit Function
EH:
    M4245 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4246() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&HFFFFFFFF&))
    M4246 = R
    Exit Function
EH:
    M4246 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4247() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&H80000000))
    M4247 = R
    Exit Function
EH:
    M4247 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4248() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&O777))
    M4248 = R
    Exit Function
EH:
    M4248 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4249() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(&O177777))
    M4249 = R
    Exit Function
EH:
    M4249 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4250() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1E5))
    M4250 = R
    Exit Function
EH:
    M4250 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4251() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1D5))
    M4251 = R
    Exit Function
EH:
    M4251 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4252() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5E+3))
    M4252 = R
    Exit Function
EH:
    M4252 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4253() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1#))
    M4253 = R
    Exit Function
EH:
    M4253 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4254() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1!))
    M4254 = R
    Exit Function
EH:
    M4254 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4255() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1@))
    M4255 = R
    Exit Function
EH:
    M4255 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4256() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1&))
    M4256 = R
    Exit Function
EH:
    M4256 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4257() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1%))
    M4257 = R
    Exit Function
EH:
    M4257 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4258() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5@))
    M4258 = R
    Exit Function
EH:
    M4258 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4259() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(12345678901234567890))
    M4259 = R
    Exit Function
EH:
    M4259 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4260() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1234567890123))
    M4260 = R
    Exit Function
EH:
    M4260 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4261() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(0.1@ + 0.2@))
    M4261 = R
    Exit Function
EH:
    M4261 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4262() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(.5))
    M4262 = R
    Exit Function
EH:
    M4262 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4263() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(5.))
    M4263 = R
    Exit Function
EH:
    M4263 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4264() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(1.5#))
    M4264 = R
    Exit Function
EH:
    M4264 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4265() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt("32768")))
    M4265 = R
    Exit Function
EH:
    M4265 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4266() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(32767.5)))
    M4266 = R
    Exit Function
EH:
    M4266 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4267() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(32768.4)))
    M4267 = R
    Exit Function
EH:
    M4267 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4268() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-32768.5)))
    M4268 = R
    Exit Function
EH:
    M4268 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4269() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-32768.51)))
    M4269 = R
    Exit Function
EH:
    M4269 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4270() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CLng(2147483647.5)))
    M4270 = R
    Exit Function
EH:
    M4270 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4271() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CLng(2147483647.4)))
    M4271 = R
    Exit Function
EH:
    M4271 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4272() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(255.5)))
    M4272 = R
    Exit Function
EH:
    M4272 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4273() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(255.4)))
    M4273 = R
    Exit Function
EH:
    M4273 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4274() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(-0.5)))
    M4274 = R
    Exit Function
EH:
    M4274 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4275() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(-0.6)))
    M4275 = R
    Exit Function
EH:
    M4275 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4276() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(0.5)))
    M4276 = R
    Exit Function
EH:
    M4276 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4277() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CByte(1.5)))
    M4277 = R
    Exit Function
EH:
    M4277 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4278() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(0.5)))
    M4278 = R
    Exit Function
EH:
    M4278 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4279() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(1.5)))
    M4279 = R
    Exit Function
EH:
    M4279 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4280() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(2.5)))
    M4280 = R
    Exit Function
EH:
    M4280 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4281() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-0.5)))
    M4281 = R
    Exit Function
EH:
    M4281 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4282() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-1.5)))
    M4282 = R
    Exit Function
EH:
    M4282 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4283() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(-2.5)))
    M4283 = R
    Exit Function
EH:
    M4283 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4284() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(922337203685477.58075)))
    M4284 = R
    Exit Function
EH:
    M4284 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4285() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(922337203685477.5808)))
    M4285 = R
    Exit Function
EH:
    M4285 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4286() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(1E15)))
    M4286 = R
    Exit Function
EH:
    M4286 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4287() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(0.00005)))
    M4287 = R
    Exit Function
EH:
    M4287 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4288() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(0.00015)))
    M4288 = R
    Exit Function
EH:
    M4288 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4289() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(0.00025)))
    M4289 = R
    Exit Function
EH:
    M4289 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4290() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur(-0.00005)))
    M4290 = R
    Exit Function
EH:
    M4290 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4291() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CCur("1.23456")))
    M4291 = R
    Exit Function
EH:
    M4291 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4292() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1E28)))
    M4292 = R
    Exit Function
EH:
    M4292 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4293() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1E29)))
    M4293 = R
    Exit Function
EH:
    M4293 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4294() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(7.9228162514264337593543950335E+28)))
    M4294 = R
    Exit Function
EH:
    M4294 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4295() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(7.9228162514264337593543950336E+28)))
    M4295 = R
    Exit Function
EH:
    M4295 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4296() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(0.1)))
    M4296 = R
    Exit Function
EH:
    M4296 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4297() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1E-28)))
    M4297 = R
    Exit Function
EH:
    M4297 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4298() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1E-29)))
    M4298 = R
    Exit Function
EH:
    M4298 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4299() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec("0.00000000000000000000000000001")))
    M4299 = R
    Exit Function
EH:
    M4299 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4300() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(-1.5)))
    M4300 = R
    Exit Function
EH:
    M4300 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4301() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1) + CDec(0.1)))
    M4301 = R
    Exit Function
EH:
    M4301 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4302() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(79228162514264337593543950335) + 1))
    M4302 = R
    Exit Function
EH:
    M4302 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4303() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(79228162514264337593543950335) - 1))
    M4303 = R
    Exit Function
EH:
    M4303 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4304() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1E28) * 10))
    M4304 = R
    Exit Function
EH:
    M4304 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4305() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1E28) / 10))
    M4305 = R
    Exit Function
EH:
    M4305 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4306() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(1) / CDec(3)))
    M4306 = R
    Exit Function
EH:
    M4306 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4307() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(2) ^ 2))
    M4307 = R
    Exit Function
EH:
    M4307 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4308() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) \ 2))
    M4308 = R
    Exit Function
EH:
    M4308 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4309() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDec(5) Mod 2))
    M4309 = R
    Exit Function
EH:
    M4309 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4310() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(3.4028235E+38)))
    M4310 = R
    Exit Function
EH:
    M4310 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4311() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(3.5E+38)))
    M4311 = R
    Exit Function
EH:
    M4311 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4312() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(1E-46)))
    M4312 = R
    Exit Function
EH:
    M4312 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4313() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(1E-45)))
    M4313 = R
    Exit Function
EH:
    M4313 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4314() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(7E-46)))
    M4314 = R
    Exit Function
EH:
    M4314 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4315() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(-1E-46)))
    M4315 = R
    Exit Function
EH:
    M4315 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4316() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDbl(CSng(1.1))))
    M4316 = R
    Exit Function
EH:
    M4316 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4317() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(1.1) = 1.1))
    M4317 = R
    Exit Function
EH:
    M4317 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4318() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(1) / 3))
    M4318 = R
    Exit Function
EH:
    M4318 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4319() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(16777217)))
    M4319 = R
    Exit Function
EH:
    M4319 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4320() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng(0.1) * 3))
    M4320 = R
    Exit Function
EH:
    M4320 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4321() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CSng("1e39")))
    M4321 = R
    Exit Function
EH:
    M4321 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4322() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(-1.5)))
    M4322 = R
    Exit Function
EH:
    M4322 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4323() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Fix(-1.5)))
    M4323 = R
    Exit Function
EH:
    M4323 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4324() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(1.5)))
    M4324 = R
    Exit Function
EH:
    M4324 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4325() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Fix(1.5)))
    M4325 = R
    Exit Function
EH:
    M4325 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4326() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(-0.5)))
    M4326 = R
    Exit Function
EH:
    M4326 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4327() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Fix(-0.5)))
    M4327 = R
    Exit Function
EH:
    M4327 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4328() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int("1.5")))
    M4328 = R
    Exit Function
EH:
    M4328 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4329() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(Null)))
    M4329 = R
    Exit Function
EH:
    M4329 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4330() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(Empty)))
    M4330 = R
    Exit Function
EH:
    M4330 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4331() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(True)))
    M4331 = R
    Exit Function
EH:
    M4331 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4332() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int("a")))
    M4332 = R
    Exit Function
EH:
    M4332 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4333() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(CDec(-1.5))))
    M4333 = R
    Exit Function
EH:
    M4333 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4334() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(CCur(-1.5))))
    M4334 = R
    Exit Function
EH:
    M4334 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4335() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Fix(CCur(-1.5))))
    M4335 = R
    Exit Function
EH:
    M4335 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4336() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(CSng(-1.5))))
    M4336 = R
    Exit Function
EH:
    M4336 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4337() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(#1/1/2000 6:00#)))
    M4337 = R
    Exit Function
EH:
    M4337 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4338() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(-0)))
    M4338 = R
    Exit Function
EH:
    M4338 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4339() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Fix(1E300)))
    M4339 = R
    Exit Function
EH:
    M4339 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4340() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Int(2 ^ 62 + 0.5)))
    M4340 = R
    Exit Function
EH:
    M4340 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4341() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(-5)))
    M4341 = R
    Exit Function
EH:
    M4341 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4342() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs("-5")))
    M4342 = R
    Exit Function
EH:
    M4342 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4343() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(Null)))
    M4343 = R
    Exit Function
EH:
    M4343 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4344() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(Empty)))
    M4344 = R
    Exit Function
EH:
    M4344 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4345() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(True)))
    M4345 = R
    Exit Function
EH:
    M4345 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4346() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(-32768)))
    M4346 = R
    Exit Function
EH:
    M4346 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4347() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(CInt(-32768))))
    M4347 = R
    Exit Function
EH:
    M4347 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4348() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(-2147483648#)))
    M4348 = R
    Exit Function
EH:
    M4348 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4349() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(CCur(-5))))
    M4349 = R
    Exit Function
EH:
    M4349 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4350() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(CDec(-5))))
    M4350 = R
    Exit Function
EH:
    M4350 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4351() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(#1/1/2000#)))
    M4351 = R
    Exit Function
EH:
    M4351 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4352() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs("a")))
    M4352 = R
    Exit Function
EH:
    M4352 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4353() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(-0)))
    M4353 = R
    Exit Function
EH:
    M4353 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4354() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Abs(CSng(-1.5))))
    M4354 = R
    Exit Function
EH:
    M4354 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4355() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(-5)))
    M4355 = R
    Exit Function
EH:
    M4355 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4356() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(0)))
    M4356 = R
    Exit Function
EH:
    M4356 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4357() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(0.1)))
    M4357 = R
    Exit Function
EH:
    M4357 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4358() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(-0.0)))
    M4358 = R
    Exit Function
EH:
    M4358 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4359() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn("5")))
    M4359 = R
    Exit Function
EH:
    M4359 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4360() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(Null)))
    M4360 = R
    Exit Function
EH:
    M4360 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4361() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(True)))
    M4361 = R
    Exit Function
EH:
    M4361 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4362() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(CDec(-5))))
    M4362 = R
    Exit Function
EH:
    M4362 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4363() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sgn(1E-300)))
    M4363 = R
    Exit Function
EH:
    M4363 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4364() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sqr(4)))
    M4364 = R
    Exit Function
EH:
    M4364 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4365() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sqr(-1)))
    M4365 = R
    Exit Function
EH:
    M4365 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4366() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sqr(0)))
    M4366 = R
    Exit Function
EH:
    M4366 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4367() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sqr("4")))
    M4367 = R
    Exit Function
EH:
    M4367 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4368() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sqr(Null)))
    M4368 = R
    Exit Function
EH:
    M4368 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4369() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Log(0)))
    M4369 = R
    Exit Function
EH:
    M4369 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4370() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Log(-1)))
    M4370 = R
    Exit Function
EH:
    M4370 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4371() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Log(1)))
    M4371 = R
    Exit Function
EH:
    M4371 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4372() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Exp(710)))
    M4372 = R
    Exit Function
EH:
    M4372 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4373() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Exp(709)))
    M4373 = R
    Exit Function
EH:
    M4373 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4374() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Exp(-746)))
    M4374 = R
    Exit Function
EH:
    M4374 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4375() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Atn(1) * 4))
    M4375 = R
    Exit Function
EH:
    M4375 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4376() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Sin(1E22)))
    M4376 = R
    Exit Function
EH:
    M4376 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4377() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Cos(1E300)))
    M4377 = R
    Exit Function
EH:
    M4377 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4378() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Tan(1.5707963267948966)))
    M4378 = R
    Exit Function
EH:
    M4378 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4379() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(2.5)))
    M4379 = R
    Exit Function
EH:
    M4379 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4380() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(3.5)))
    M4380 = R
    Exit Function
EH:
    M4380 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4381() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(-2.5)))
    M4381 = R
    Exit Function
EH:
    M4381 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4382() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(2.675, 2)))
    M4382 = R
    Exit Function
EH:
    M4382 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4383() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1.005, 2)))
    M4383 = R
    Exit Function
EH:
    M4383 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4384() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(0.125, 2)))
    M4384 = R
    Exit Function
EH:
    M4384 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4385() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1234.5678, -2)))
    M4385 = R
    Exit Function
EH:
    M4385 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4386() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1.5, 0)))
    M4386 = R
    Exit Function
EH:
    M4386 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4387() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1E15 + 0.5)))
    M4387 = R
    Exit Function
EH:
    M4387 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4388() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(0.5)))
    M4388 = R
    Exit Function
EH:
    M4388 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4389() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(-0.5)))
    M4389 = R
    Exit Function
EH:
    M4389 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4390() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(2.5, 1)))
    M4390 = R
    Exit Function
EH:
    M4390 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4391() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(Null)))
    M4391 = R
    Exit Function
EH:
    M4391 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4392() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round("2.5")))
    M4392 = R
    Exit Function
EH:
    M4392 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4393() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(2.5, "a")))
    M4393 = R
    Exit Function
EH:
    M4393 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4394() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(CDec(2.5))))
    M4394 = R
    Exit Function
EH:
    M4394 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4395() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(CCur(2.5))))
    M4395 = R
    Exit Function
EH:
    M4395 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4396() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(CCur(0.125), 2)))
    M4396 = R
    Exit Function
EH:
    M4396 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4397() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(CDec(0.125), 2)))
    M4397 = R
    Exit Function
EH:
    M4397 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4398() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(5, 400)))
    M4398 = R
    Exit Function
EH:
    M4398 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4399() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(5, -400)))
    M4399 = R
    Exit Function
EH:
    M4399 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4400() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1E308, -300)))
    M4400 = R
    Exit Function
EH:
    M4400 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4401() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(0.285, 2)))
    M4401 = R
    Exit Function
EH:
    M4401 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4402() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1.45, 1)))
    M4402 = R
    Exit Function
EH:
    M4402 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4403() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(-1.45, 1)))
    M4403 = R
    Exit Function
EH:
    M4403 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4404() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(2.5, 40)))
    M4404 = R
    Exit Function
EH:
    M4404 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4405() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1.1, 15)))
    M4405 = R
    Exit Function
EH:
    M4405 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4406() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1.1, 16)))
    M4406 = R
    Exit Function
EH:
    M4406 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4407() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1234567.891, 2)))
    M4407 = R
    Exit Function
EH:
    M4407 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4408() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(8.345, 2)))
    M4408 = R
    Exit Function
EH:
    M4408 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4409() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(10.275, 2)))
    M4409 = R
    Exit Function
EH:
    M4409 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4410() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(1.355, 2)))
    M4410 = R
    Exit Function
EH:
    M4410 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4411() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(0.5, 3)))
    M4411 = R
    Exit Function
EH:
    M4411 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4412() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(CSng(2.675), 2)))
    M4412 = R
    Exit Function
EH:
    M4412 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4413() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(True)))
    M4413 = R
    Exit Function
EH:
    M4413 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4414() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Round(#1/1/2000 6:00#)))
    M4414 = R
    Exit Function
EH:
    M4414 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4415() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round(2.5))))
    M4415 = R
    Exit Function
EH:
    M4415 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4416() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round(CInt(2)))))
    M4416 = R
    Exit Function
EH:
    M4416 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4417() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round(CSng(2.5)))))
    M4417 = R
    Exit Function
EH:
    M4417 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4418() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round(CCur(2.5)))))
    M4418 = R
    Exit Function
EH:
    M4418 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4419() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round(CDec(2.5)))))
    M4419 = R
    Exit Function
EH:
    M4419 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4420() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round("2"))))
    M4420 = R
    Exit Function
EH:
    M4420 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4421() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Round(True))))
    M4421 = R
    Exit Function
EH:
    M4421 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4422() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(255)))
    M4422 = R
    Exit Function
EH:
    M4422 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4423() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(-1)))
    M4423 = R
    Exit Function
EH:
    M4423 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4424() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CInt(-1))))
    M4424 = R
    Exit Function
EH:
    M4424 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4425() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CByte(255))))
    M4425 = R
    Exit Function
EH:
    M4425 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4426() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(2147483648#)))
    M4426 = R
    Exit Function
EH:
    M4426 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4427() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(-2147483649#)))
    M4427 = R
    Exit Function
EH:
    M4427 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4428() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(4294967295#)))
    M4428 = R
    Exit Function
EH:
    M4428 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4429() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(2 ^ 31)))
    M4429 = R
    Exit Function
EH:
    M4429 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4430() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(Null)))
    M4430 = R
    Exit Function
EH:
    M4430 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4431() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(Empty)))
    M4431 = R
    Exit Function
EH:
    M4431 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4432() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex("255")))
    M4432 = R
    Exit Function
EH:
    M4432 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4433() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex("a")))
    M4433 = R
    Exit Function
EH:
    M4433 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4434() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(1.5)))
    M4434 = R
    Exit Function
EH:
    M4434 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4435() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(2.5)))
    M4435 = R
    Exit Function
EH:
    M4435 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4436() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(0.5)))
    M4436 = R
    Exit Function
EH:
    M4436 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4437() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(True)))
    M4437 = R
    Exit Function
EH:
    M4437 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4438() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CDec(255))))
    M4438 = R
    Exit Function
EH:
    M4438 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4439() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CCur(255))))
    M4439 = R
    Exit Function
EH:
    M4439 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4440() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CSng(255))))
    M4440 = R
    Exit Function
EH:
    M4440 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4441() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Oct(8)))
    M4441 = R
    Exit Function
EH:
    M4441 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4442() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Oct(-1)))
    M4442 = R
    Exit Function
EH:
    M4442 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4443() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Oct(CInt(-1))))
    M4443 = R
    Exit Function
EH:
    M4443 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4444() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Oct(0)))
    M4444 = R
    Exit Function
EH:
    M4444 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4445() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Oct(2147483647)))
    M4445 = R
    Exit Function
EH:
    M4445 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4446() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Oct(Null)))
    M4446 = R
    Exit Function
EH:
    M4446 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4447() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Hex(5))))
    M4447 = R
    Exit Function
EH:
    M4447 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4448() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Hex$(5))))
    M4448 = R
    Exit Function
EH:
    M4448 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4449() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Hex(Null))))
    M4449 = R
    Exit Function
EH:
    M4449 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4450() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(Oct(Empty))))
    M4450 = R
    Exit Function
EH:
    M4450 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4451() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(-32768)))
    M4451 = R
    Exit Function
EH:
    M4451 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4452() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(32768)))
    M4452 = R
    Exit Function
EH:
    M4452 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4453() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CByte(0))))
    M4453 = R
    Exit Function
EH:
    M4453 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4454() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(CLng(-2147483648#))))
    M4454 = R
    Exit Function
EH:
    M4454 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4455() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(1E10)))
    M4455 = R
    Exit Function
EH:
    M4455 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4456() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(-1E10)))
    M4456 = R
    Exit Function
EH:
    M4456 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4457() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(4294967296#)))
    M4457 = R
    Exit Function
EH:
    M4457 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4458() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(4294967295.5)))
    M4458 = R
    Exit Function
EH:
    M4458 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4459() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(-2147483648.5)))
    M4459 = R
    Exit Function
EH:
    M4459 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4460() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(-2147483648.6)))
    M4460 = R
    Exit Function
EH:
    M4460 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4461() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(2147483647.5)))
    M4461 = R
    Exit Function
EH:
    M4461 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4462() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Hex(2147483647.4)))
    M4462 = R
    Exit Function
EH:
    M4462 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4463() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(0)))
    M4463 = R
    Exit Function
EH:
    M4463 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4464() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("0")))
    M4464 = R
    Exit Function
EH:
    M4464 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4465() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("False")))
    M4465 = R
    Exit Function
EH:
    M4465 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4466() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("false")))
    M4466 = R
    Exit Function
EH:
    M4466 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4467() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("FALSE")))
    M4467 = R
    Exit Function
EH:
    M4467 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4468() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("True")))
    M4468 = R
    Exit Function
EH:
    M4468 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4469() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("yes")))
    M4469 = R
    Exit Function
EH:
    M4469 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4470() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("")))
    M4470 = R
    Exit Function
EH:
    M4470 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4471() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(" ")))
    M4471 = R
    Exit Function
EH:
    M4471 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4472() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("0.0")))
    M4472 = R
    Exit Function
EH:
    M4472 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4473() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("-0")))
    M4473 = R
    Exit Function
EH:
    M4473 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4474() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(0.0001)))
    M4474 = R
    Exit Function
EH:
    M4474 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4475() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(-0.0001)))
    M4475 = R
    Exit Function
EH:
    M4475 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4476() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(Null)))
    M4476 = R
    Exit Function
EH:
    M4476 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4477() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(Empty)))
    M4477 = R
    Exit Function
EH:
    M4477 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4478() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(CVErr(5))))
    M4478 = R
    Exit Function
EH:
    M4478 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4479() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(#12:00:00 AM#)))
    M4479 = R
    Exit Function
EH:
    M4479 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4480() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("1e-400")))
    M4480 = R
    Exit Function
EH:
    M4480 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4481() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("&H0")))
    M4481 = R
    Exit Function
EH:
    M4481 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4482() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("&H1")))
    M4482 = R
    Exit Function
EH:
    M4482 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4483() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool("#FALSE#")))
    M4483 = R
    Exit Function
EH:
    M4483 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4484() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVar(1)))
    M4484 = R
    Exit Function
EH:
    M4484 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4485() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVar(Null)))
    M4485 = R
    Exit Function
EH:
    M4485 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4486() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVar(Empty)))
    M4486 = R
    Exit Function
EH:
    M4486 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4487() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVar("a")))
    M4487 = R
    Exit Function
EH:
    M4487 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4488() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVar(CVErr(3))))
    M4488 = R
    Exit Function
EH:
    M4488 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4489() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(0)))
    M4489 = R
    Exit Function
EH:
    M4489 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4490() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(-1)))
    M4490 = R
    Exit Function
EH:
    M4490 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4491() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(65535)))
    M4491 = R
    Exit Function
EH:
    M4491 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4492() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(65536)))
    M4492 = R
    Exit Function
EH:
    M4492 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4493() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(2007)))
    M4493 = R
    Exit Function
EH:
    M4493 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4494() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr("a")))
    M4494 = R
    Exit Function
EH:
    M4494 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4495() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(Null)))
    M4495 = R
    Exit Function
EH:
    M4495 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4496() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(1.5)))
    M4496 = R
    Exit Function
EH:
    M4496 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4497() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(CVErr(7))))
    M4497 = R
    Exit Function
EH:
    M4497 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4498() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(2042) & ""))
    M4498 = R
    Exit Function
EH:
    M4498 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4499() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(2042) + 1))
    M4499 = R
    Exit Function
EH:
    M4499 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4500() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(2042) = 2042))
    M4500 = R
    Exit Function
EH:
    M4500 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4501() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CStr(CVErr(2042))))
    M4501 = R
    Exit Function
EH:
    M4501 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4502() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(TypeName(CVErr(2042))))
    M4502 = R
    Exit Function
EH:
    M4502 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4503() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(VarType(CVErr(2042))))
    M4503 = R
    Exit Function
EH:
    M4503 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4504() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsError(CVErr(1))))
    M4504 = R
    Exit Function
EH:
    M4504 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4505() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsError(1)))
    M4505 = R
    Exit Function
EH:
    M4505 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4506() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Val(CVErr(2042))))
    M4506 = R
    Exit Function
EH:
    M4506 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4507() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CInt(CVErr(2042))))
    M4507 = R
    Exit Function
EH:
    M4507 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4508() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Len(CVErr(2042))))
    M4508 = R
    Exit Function
EH:
    M4508 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4509() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(UCase(CVErr(2042))))
    M4509 = R
    Exit Function
EH:
    M4509 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4510() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Not CVErr(2042)))
    M4510 = R
    Exit Function
EH:
    M4510 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4511() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(-CVErr(2042)))
    M4511 = R
    Exit Function
EH:
    M4511 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4512() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CVErr(2042) And 1))
    M4512 = R
    Exit Function
EH:
    M4512 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4513() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsNumeric(CVErr(2042))))
    M4513 = R
    Exit Function
EH:
    M4513 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4514() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsNull(CVErr(2042))))
    M4514 = R
    Exit Function
EH:
    M4514 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4515() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsEmpty(CVErr(2042))))
    M4515 = R
    Exit Function
EH:
    M4515 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4516() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsDate(CVErr(2042))))
    M4516 = R
    Exit Function
EH:
    M4516 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4517() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsObject(CVErr(2042))))
    M4517 = R
    Exit Function
EH:
    M4517 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4518() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(IsArray(CVErr(2042))))
    M4518 = R
    Exit Function
EH:
    M4518 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4519() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDbl(CVErr(2042))))
    M4519 = R
    Exit Function
EH:
    M4519 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4520() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CDate(CVErr(2042))))
    M4520 = R
    Exit Function
EH:
    M4520 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4521() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(CBool(CVErr(2042))))
    M4521 = R
    Exit Function
EH:
    M4521 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4522() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4522 = R
    Exit Function
EH:
    M4522 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4523() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4523 = R
    Exit Function
EH:
    M4523 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4524() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4524 = R
    Exit Function
EH:
    M4524 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4525() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4525 = R
    Exit Function
EH:
    M4525 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4526() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4526 = R
    Exit Function
EH:
    M4526 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4527() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4527 = R
    Exit Function
EH:
    M4527 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4528() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4528 = R
    Exit Function
EH:
    M4528 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4529() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4529 = R
    Exit Function
EH:
    M4529 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4530() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4530 = R
    Exit Function
EH:
    M4530 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4531() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Variant
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4531 = R
    Exit Function
EH:
    M4531 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4532() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Object
    R = TypeName(x) & "|" & VarType(x) & "|" & Dump(fV(x))
    M4532 = R
    Exit Function
EH:
    M4532 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4533() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4533 = R
    Exit Function
EH:
    M4533 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4534() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4534 = R
    Exit Function
EH:
    M4534 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4535() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4535 = R
    Exit Function
EH:
    M4535 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4536() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4536 = R
    Exit Function
EH:
    M4536 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4537() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4537 = R
    Exit Function
EH:
    M4537 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4538() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4538 = R
    Exit Function
EH:
    M4538 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4539() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4539 = R
    Exit Function
EH:
    M4539 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4540() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4540 = R
    Exit Function
EH:
    M4540 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4541() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4541 = R
    Exit Function
EH:
    M4541 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4542() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4542 = R
    Exit Function
EH:
    M4542 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4543() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4543 = R
    Exit Function
EH:
    M4543 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4544() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4544 = R
    Exit Function
EH:
    M4544 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4545() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4545 = R
    Exit Function
EH:
    M4545 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4546() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4546 = R
    Exit Function
EH:
    M4546 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4547() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4547 = R
    Exit Function
EH:
    M4547 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4548() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4548 = R
    Exit Function
EH:
    M4548 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4549() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4549 = R
    Exit Function
EH:
    M4549 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4550() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4550 = R
    Exit Function
EH:
    M4550 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4551() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4551 = R
    Exit Function
EH:
    M4551 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4552() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4552 = R
    Exit Function
EH:
    M4552 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4553() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4553 = R
    Exit Function
EH:
    M4553 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4554() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4554 = R
    Exit Function
EH:
    M4554 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4555() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4555 = R
    Exit Function
EH:
    M4555 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4556() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4556 = R
    Exit Function
EH:
    M4556 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4557() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4557 = R
    Exit Function
EH:
    M4557 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4558() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4558 = R
    Exit Function
EH:
    M4558 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4559() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4559 = R
    Exit Function
EH:
    M4559 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4560() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4560 = R
    Exit Function
EH:
    M4560 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4561() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4561 = R
    Exit Function
EH:
    M4561 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4562() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4562 = R
    Exit Function
EH:
    M4562 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4563() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4563 = R
    Exit Function
EH:
    M4563 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4564() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4564 = R
    Exit Function
EH:
    M4564 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4565() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4565 = R
    Exit Function
EH:
    M4565 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4566() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4566 = R
    Exit Function
EH:
    M4566 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4567() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4567 = R
    Exit Function
EH:
    M4567 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4568() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Integer
    x = fV("")
    R = TypeName(x) & ":" & x
    M4568 = R
    Exit Function
EH:
    M4568 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4569() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4569 = R
    Exit Function
EH:
    M4569 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4570() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4570 = R
    Exit Function
EH:
    M4570 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4571() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4571 = R
    Exit Function
EH:
    M4571 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4572() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4572 = R
    Exit Function
EH:
    M4572 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4573() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4573 = R
    Exit Function
EH:
    M4573 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4574() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4574 = R
    Exit Function
EH:
    M4574 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4575() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4575 = R
    Exit Function
EH:
    M4575 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4576() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4576 = R
    Exit Function
EH:
    M4576 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4577() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4577 = R
    Exit Function
EH:
    M4577 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4578() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4578 = R
    Exit Function
EH:
    M4578 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4579() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4579 = R
    Exit Function
EH:
    M4579 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4580() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4580 = R
    Exit Function
EH:
    M4580 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4581() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4581 = R
    Exit Function
EH:
    M4581 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4582() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4582 = R
    Exit Function
EH:
    M4582 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4583() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4583 = R
    Exit Function
EH:
    M4583 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4584() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4584 = R
    Exit Function
EH:
    M4584 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4585() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4585 = R
    Exit Function
EH:
    M4585 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4586() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4586 = R
    Exit Function
EH:
    M4586 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4587() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4587 = R
    Exit Function
EH:
    M4587 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4588() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4588 = R
    Exit Function
EH:
    M4588 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4589() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4589 = R
    Exit Function
EH:
    M4589 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4590() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4590 = R
    Exit Function
EH:
    M4590 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4591() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4591 = R
    Exit Function
EH:
    M4591 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4592() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4592 = R
    Exit Function
EH:
    M4592 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4593() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4593 = R
    Exit Function
EH:
    M4593 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4594() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4594 = R
    Exit Function
EH:
    M4594 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4595() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4595 = R
    Exit Function
EH:
    M4595 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4596() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4596 = R
    Exit Function
EH:
    M4596 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4597() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4597 = R
    Exit Function
EH:
    M4597 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4598() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4598 = R
    Exit Function
EH:
    M4598 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4599() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4599 = R
    Exit Function
EH:
    M4599 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4600() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4600 = R
    Exit Function
EH:
    M4600 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4601() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4601 = R
    Exit Function
EH:
    M4601 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4602() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4602 = R
    Exit Function
EH:
    M4602 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4603() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4603 = R
    Exit Function
EH:
    M4603 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4604() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Long
    x = fV("")
    R = TypeName(x) & ":" & x
    M4604 = R
    Exit Function
EH:
    M4604 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4605() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4605 = R
    Exit Function
EH:
    M4605 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4606() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4606 = R
    Exit Function
EH:
    M4606 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4607() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4607 = R
    Exit Function
EH:
    M4607 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4608() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4608 = R
    Exit Function
EH:
    M4608 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4609() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4609 = R
    Exit Function
EH:
    M4609 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4610() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4610 = R
    Exit Function
EH:
    M4610 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4611() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4611 = R
    Exit Function
EH:
    M4611 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4612() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4612 = R
    Exit Function
EH:
    M4612 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4613() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4613 = R
    Exit Function
EH:
    M4613 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4614() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4614 = R
    Exit Function
EH:
    M4614 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4615() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4615 = R
    Exit Function
EH:
    M4615 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4616() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4616 = R
    Exit Function
EH:
    M4616 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4617() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4617 = R
    Exit Function
EH:
    M4617 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4618() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4618 = R
    Exit Function
EH:
    M4618 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4619() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4619 = R
    Exit Function
EH:
    M4619 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4620() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4620 = R
    Exit Function
EH:
    M4620 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4621() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4621 = R
    Exit Function
EH:
    M4621 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4622() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4622 = R
    Exit Function
EH:
    M4622 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4623() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4623 = R
    Exit Function
EH:
    M4623 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4624() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4624 = R
    Exit Function
EH:
    M4624 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4625() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4625 = R
    Exit Function
EH:
    M4625 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4626() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4626 = R
    Exit Function
EH:
    M4626 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4627() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4627 = R
    Exit Function
EH:
    M4627 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4628() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4628 = R
    Exit Function
EH:
    M4628 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4629() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4629 = R
    Exit Function
EH:
    M4629 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4630() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4630 = R
    Exit Function
EH:
    M4630 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4631() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4631 = R
    Exit Function
EH:
    M4631 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4632() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4632 = R
    Exit Function
EH:
    M4632 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4633() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4633 = R
    Exit Function
EH:
    M4633 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4634() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4634 = R
    Exit Function
EH:
    M4634 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4635() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4635 = R
    Exit Function
EH:
    M4635 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4636() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4636 = R
    Exit Function
EH:
    M4636 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4637() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4637 = R
    Exit Function
EH:
    M4637 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4638() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4638 = R
    Exit Function
EH:
    M4638 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4639() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4639 = R
    Exit Function
EH:
    M4639 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4640() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Byte
    x = fV("")
    R = TypeName(x) & ":" & x
    M4640 = R
    Exit Function
EH:
    M4640 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4641() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4641 = R
    Exit Function
EH:
    M4641 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4642() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4642 = R
    Exit Function
EH:
    M4642 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4643() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4643 = R
    Exit Function
EH:
    M4643 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4644() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4644 = R
    Exit Function
EH:
    M4644 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4645() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4645 = R
    Exit Function
EH:
    M4645 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4646() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4646 = R
    Exit Function
EH:
    M4646 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4647() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4647 = R
    Exit Function
EH:
    M4647 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4648() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4648 = R
    Exit Function
EH:
    M4648 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4649() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4649 = R
    Exit Function
EH:
    M4649 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4650() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4650 = R
    Exit Function
EH:
    M4650 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4651() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4651 = R
    Exit Function
EH:
    M4651 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4652() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4652 = R
    Exit Function
EH:
    M4652 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4653() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4653 = R
    Exit Function
EH:
    M4653 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4654() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4654 = R
    Exit Function
EH:
    M4654 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4655() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4655 = R
    Exit Function
EH:
    M4655 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4656() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4656 = R
    Exit Function
EH:
    M4656 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4657() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4657 = R
    Exit Function
EH:
    M4657 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4658() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4658 = R
    Exit Function
EH:
    M4658 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4659() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4659 = R
    Exit Function
EH:
    M4659 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4660() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4660 = R
    Exit Function
EH:
    M4660 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4661() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4661 = R
    Exit Function
EH:
    M4661 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4662() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4662 = R
    Exit Function
EH:
    M4662 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4663() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4663 = R
    Exit Function
EH:
    M4663 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4664() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4664 = R
    Exit Function
EH:
    M4664 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4665() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4665 = R
    Exit Function
EH:
    M4665 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4666() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4666 = R
    Exit Function
EH:
    M4666 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4667() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4667 = R
    Exit Function
EH:
    M4667 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4668() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4668 = R
    Exit Function
EH:
    M4668 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4669() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4669 = R
    Exit Function
EH:
    M4669 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4670() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4670 = R
    Exit Function
EH:
    M4670 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4671() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4671 = R
    Exit Function
EH:
    M4671 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4672() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4672 = R
    Exit Function
EH:
    M4672 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4673() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4673 = R
    Exit Function
EH:
    M4673 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4674() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4674 = R
    Exit Function
EH:
    M4674 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4675() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4675 = R
    Exit Function
EH:
    M4675 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4676() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Single
    x = fV("")
    R = TypeName(x) & ":" & x
    M4676 = R
    Exit Function
EH:
    M4676 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4677() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4677 = R
    Exit Function
EH:
    M4677 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4678() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4678 = R
    Exit Function
EH:
    M4678 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4679() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4679 = R
    Exit Function
EH:
    M4679 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4680() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4680 = R
    Exit Function
EH:
    M4680 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4681() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4681 = R
    Exit Function
EH:
    M4681 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4682() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4682 = R
    Exit Function
EH:
    M4682 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4683() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4683 = R
    Exit Function
EH:
    M4683 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4684() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4684 = R
    Exit Function
EH:
    M4684 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4685() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4685 = R
    Exit Function
EH:
    M4685 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4686() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4686 = R
    Exit Function
EH:
    M4686 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4687() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4687 = R
    Exit Function
EH:
    M4687 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4688() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4688 = R
    Exit Function
EH:
    M4688 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4689() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4689 = R
    Exit Function
EH:
    M4689 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4690() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4690 = R
    Exit Function
EH:
    M4690 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4691() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4691 = R
    Exit Function
EH:
    M4691 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4692() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4692 = R
    Exit Function
EH:
    M4692 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4693() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4693 = R
    Exit Function
EH:
    M4693 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4694() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4694 = R
    Exit Function
EH:
    M4694 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4695() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4695 = R
    Exit Function
EH:
    M4695 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4696() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4696 = R
    Exit Function
EH:
    M4696 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4697() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4697 = R
    Exit Function
EH:
    M4697 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4698() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4698 = R
    Exit Function
EH:
    M4698 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4699() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4699 = R
    Exit Function
EH:
    M4699 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4700() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4700 = R
    Exit Function
EH:
    M4700 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4701() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4701 = R
    Exit Function
EH:
    M4701 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4702() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4702 = R
    Exit Function
EH:
    M4702 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4703() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4703 = R
    Exit Function
EH:
    M4703 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4704() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4704 = R
    Exit Function
EH:
    M4704 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4705() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4705 = R
    Exit Function
EH:
    M4705 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4706() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4706 = R
    Exit Function
EH:
    M4706 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4707() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4707 = R
    Exit Function
EH:
    M4707 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4708() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4708 = R
    Exit Function
EH:
    M4708 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4709() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4709 = R
    Exit Function
EH:
    M4709 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4710() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4710 = R
    Exit Function
EH:
    M4710 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4711() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4711 = R
    Exit Function
EH:
    M4711 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4712() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Double
    x = fV("")
    R = TypeName(x) & ":" & x
    M4712 = R
    Exit Function
EH:
    M4712 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4713() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4713 = R
    Exit Function
EH:
    M4713 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4714() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4714 = R
    Exit Function
EH:
    M4714 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4715() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4715 = R
    Exit Function
EH:
    M4715 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4716() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4716 = R
    Exit Function
EH:
    M4716 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4717() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4717 = R
    Exit Function
EH:
    M4717 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4718() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4718 = R
    Exit Function
EH:
    M4718 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4719() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4719 = R
    Exit Function
EH:
    M4719 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4720() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4720 = R
    Exit Function
EH:
    M4720 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4721() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4721 = R
    Exit Function
EH:
    M4721 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4722() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4722 = R
    Exit Function
EH:
    M4722 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4723() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4723 = R
    Exit Function
EH:
    M4723 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4724() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4724 = R
    Exit Function
EH:
    M4724 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4725() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4725 = R
    Exit Function
EH:
    M4725 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4726() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4726 = R
    Exit Function
EH:
    M4726 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4727() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4727 = R
    Exit Function
EH:
    M4727 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4728() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4728 = R
    Exit Function
EH:
    M4728 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4729() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4729 = R
    Exit Function
EH:
    M4729 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4730() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4730 = R
    Exit Function
EH:
    M4730 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4731() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4731 = R
    Exit Function
EH:
    M4731 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4732() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4732 = R
    Exit Function
EH:
    M4732 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4733() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4733 = R
    Exit Function
EH:
    M4733 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4734() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4734 = R
    Exit Function
EH:
    M4734 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4735() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4735 = R
    Exit Function
EH:
    M4735 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4736() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4736 = R
    Exit Function
EH:
    M4736 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4737() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4737 = R
    Exit Function
EH:
    M4737 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4738() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4738 = R
    Exit Function
EH:
    M4738 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4739() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4739 = R
    Exit Function
EH:
    M4739 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4740() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4740 = R
    Exit Function
EH:
    M4740 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4741() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4741 = R
    Exit Function
EH:
    M4741 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4742() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4742 = R
    Exit Function
EH:
    M4742 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4743() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4743 = R
    Exit Function
EH:
    M4743 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4744() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4744 = R
    Exit Function
EH:
    M4744 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4745() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4745 = R
    Exit Function
EH:
    M4745 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4746() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4746 = R
    Exit Function
EH:
    M4746 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4747() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4747 = R
    Exit Function
EH:
    M4747 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4748() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Currency
    x = fV("")
    R = TypeName(x) & ":" & x
    M4748 = R
    Exit Function
EH:
    M4748 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4749() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4749 = R
    Exit Function
EH:
    M4749 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4750() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4750 = R
    Exit Function
EH:
    M4750 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4751() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4751 = R
    Exit Function
EH:
    M4751 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4752() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4752 = R
    Exit Function
EH:
    M4752 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4753() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4753 = R
    Exit Function
EH:
    M4753 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4754() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4754 = R
    Exit Function
EH:
    M4754 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4755() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4755 = R
    Exit Function
EH:
    M4755 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4756() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4756 = R
    Exit Function
EH:
    M4756 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4757() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4757 = R
    Exit Function
EH:
    M4757 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4758() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4758 = R
    Exit Function
EH:
    M4758 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4759() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4759 = R
    Exit Function
EH:
    M4759 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4760() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4760 = R
    Exit Function
EH:
    M4760 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4761() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4761 = R
    Exit Function
EH:
    M4761 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4762() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4762 = R
    Exit Function
EH:
    M4762 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4763() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4763 = R
    Exit Function
EH:
    M4763 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4764() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4764 = R
    Exit Function
EH:
    M4764 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4765() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4765 = R
    Exit Function
EH:
    M4765 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4766() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4766 = R
    Exit Function
EH:
    M4766 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4767() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4767 = R
    Exit Function
EH:
    M4767 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4768() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4768 = R
    Exit Function
EH:
    M4768 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4769() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4769 = R
    Exit Function
EH:
    M4769 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4770() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4770 = R
    Exit Function
EH:
    M4770 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4771() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4771 = R
    Exit Function
EH:
    M4771 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4772() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4772 = R
    Exit Function
EH:
    M4772 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4773() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4773 = R
    Exit Function
EH:
    M4773 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4774() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4774 = R
    Exit Function
EH:
    M4774 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4775() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4775 = R
    Exit Function
EH:
    M4775 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4776() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4776 = R
    Exit Function
EH:
    M4776 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4777() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4777 = R
    Exit Function
EH:
    M4777 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4778() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4778 = R
    Exit Function
EH:
    M4778 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4779() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4779 = R
    Exit Function
EH:
    M4779 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4780() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4780 = R
    Exit Function
EH:
    M4780 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4781() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4781 = R
    Exit Function
EH:
    M4781 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4782() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4782 = R
    Exit Function
EH:
    M4782 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4783() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4783 = R
    Exit Function
EH:
    M4783 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4784() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Boolean
    x = fV("")
    R = TypeName(x) & ":" & x
    M4784 = R
    Exit Function
EH:
    M4784 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4785() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4785 = R
    Exit Function
EH:
    M4785 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4786() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4786 = R
    Exit Function
EH:
    M4786 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4787() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4787 = R
    Exit Function
EH:
    M4787 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4788() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4788 = R
    Exit Function
EH:
    M4788 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4789() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4789 = R
    Exit Function
EH:
    M4789 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4790() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4790 = R
    Exit Function
EH:
    M4790 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4791() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4791 = R
    Exit Function
EH:
    M4791 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4792() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4792 = R
    Exit Function
EH:
    M4792 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4793() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4793 = R
    Exit Function
EH:
    M4793 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4794() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4794 = R
    Exit Function
EH:
    M4794 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4795() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4795 = R
    Exit Function
EH:
    M4795 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4796() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4796 = R
    Exit Function
EH:
    M4796 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4797() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4797 = R
    Exit Function
EH:
    M4797 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4798() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4798 = R
    Exit Function
EH:
    M4798 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4799() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4799 = R
    Exit Function
EH:
    M4799 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4800() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4800 = R
    Exit Function
EH:
    M4800 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4801() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4801 = R
    Exit Function
EH:
    M4801 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4802() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4802 = R
    Exit Function
EH:
    M4802 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4803() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4803 = R
    Exit Function
EH:
    M4803 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4804() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4804 = R
    Exit Function
EH:
    M4804 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4805() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4805 = R
    Exit Function
EH:
    M4805 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4806() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4806 = R
    Exit Function
EH:
    M4806 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4807() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4807 = R
    Exit Function
EH:
    M4807 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4808() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4808 = R
    Exit Function
EH:
    M4808 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4809() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4809 = R
    Exit Function
EH:
    M4809 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4810() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4810 = R
    Exit Function
EH:
    M4810 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4811() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4811 = R
    Exit Function
EH:
    M4811 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4812() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4812 = R
    Exit Function
EH:
    M4812 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4813() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4813 = R
    Exit Function
EH:
    M4813 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4814() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4814 = R
    Exit Function
EH:
    M4814 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4815() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4815 = R
    Exit Function
EH:
    M4815 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4816() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4816 = R
    Exit Function
EH:
    M4816 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4817() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4817 = R
    Exit Function
EH:
    M4817 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4818() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4818 = R
    Exit Function
EH:
    M4818 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4819() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4819 = R
    Exit Function
EH:
    M4819 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4820() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As String
    x = fV("")
    R = TypeName(x) & ":" & x
    M4820 = R
    Exit Function
EH:
    M4820 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4821() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = 5
    x = v
    R = TypeName(x) & ":" & x
    M4821 = R
    Exit Function
EH:
    M4821 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4822() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(5)
    R = TypeName(x) & ":" & x
    M4822 = R
    Exit Function
EH:
    M4822 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4823() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = "5"
    x = v
    R = TypeName(x) & ":" & x
    M4823 = R
    Exit Function
EH:
    M4823 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4824() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV("5")
    R = TypeName(x) & ":" & x
    M4824 = R
    Exit Function
EH:
    M4824 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4825() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = "a"
    x = v
    R = TypeName(x) & ":" & x
    M4825 = R
    Exit Function
EH:
    M4825 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4826() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV("a")
    R = TypeName(x) & ":" & x
    M4826 = R
    Exit Function
EH:
    M4826 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4827() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = Null
    x = v
    R = TypeName(x) & ":" & x
    M4827 = R
    Exit Function
EH:
    M4827 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4828() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(Null)
    R = TypeName(x) & ":" & x
    M4828 = R
    Exit Function
EH:
    M4828 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4829() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = Empty
    x = v
    R = TypeName(x) & ":" & x
    M4829 = R
    Exit Function
EH:
    M4829 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4830() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(Empty)
    R = TypeName(x) & ":" & x
    M4830 = R
    Exit Function
EH:
    M4830 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4831() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = 5.5
    x = v
    R = TypeName(x) & ":" & x
    M4831 = R
    Exit Function
EH:
    M4831 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4832() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(5.5)
    R = TypeName(x) & ":" & x
    M4832 = R
    Exit Function
EH:
    M4832 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4833() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = -1
    x = v
    R = TypeName(x) & ":" & x
    M4833 = R
    Exit Function
EH:
    M4833 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4834() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(-1)
    R = TypeName(x) & ":" & x
    M4834 = R
    Exit Function
EH:
    M4834 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4835() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = 300
    x = v
    R = TypeName(x) & ":" & x
    M4835 = R
    Exit Function
EH:
    M4835 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4836() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(300)
    R = TypeName(x) & ":" & x
    M4836 = R
    Exit Function
EH:
    M4836 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4837() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = 70000
    x = v
    R = TypeName(x) & ":" & x
    M4837 = R
    Exit Function
EH:
    M4837 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4838() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(70000)
    R = TypeName(x) & ":" & x
    M4838 = R
    Exit Function
EH:
    M4838 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4839() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = 1E10
    x = v
    R = TypeName(x) & ":" & x
    M4839 = R
    Exit Function
EH:
    M4839 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4840() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(1E10)
    R = TypeName(x) & ":" & x
    M4840 = R
    Exit Function
EH:
    M4840 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4841() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = True
    x = v
    R = TypeName(x) & ":" & x
    M4841 = R
    Exit Function
EH:
    M4841 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4842() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(True)
    R = TypeName(x) & ":" & x
    M4842 = R
    Exit Function
EH:
    M4842 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4843() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = #1/1/2000#
    x = v
    R = TypeName(x) & ":" & x
    M4843 = R
    Exit Function
EH:
    M4843 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4844() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(#1/1/2000#)
    R = TypeName(x) & ":" & x
    M4844 = R
    Exit Function
EH:
    M4844 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4845() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = CVErr(3)
    x = v
    R = TypeName(x) & ":" & x
    M4845 = R
    Exit Function
EH:
    M4845 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4846() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(CVErr(3))
    R = TypeName(x) & ":" & x
    M4846 = R
    Exit Function
EH:
    M4846 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4847() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = Array(1)
    x = v
    R = TypeName(x) & ":" & x
    M4847 = R
    Exit Function
EH:
    M4847 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4848() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(Array(1))
    R = TypeName(x) & ":" & x
    M4848 = R
    Exit Function
EH:
    M4848 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4849() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = Nothing
    x = v
    R = TypeName(x) & ":" & x
    M4849 = R
    Exit Function
EH:
    M4849 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4850() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(Nothing)
    R = TypeName(x) & ":" & x
    M4850 = R
    Exit Function
EH:
    M4850 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4851() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = "1e2"
    x = v
    R = TypeName(x) & ":" & x
    M4851 = R
    Exit Function
EH:
    M4851 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4852() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV("1e2")
    R = TypeName(x) & ":" & x
    M4852 = R
    Exit Function
EH:
    M4852 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4853() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = " 7 "
    x = v
    R = TypeName(x) & ":" & x
    M4853 = R
    Exit Function
EH:
    M4853 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4854() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV(" 7 ")
    R = TypeName(x) & ":" & x
    M4854 = R
    Exit Function
EH:
    M4854 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4855() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    Dim v As Variant
    v = ""
    x = v
    R = TypeName(x) & ":" & x
    M4855 = R
    Exit Function
EH:
    M4855 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4856() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim x As Date
    x = fV("")
    R = TypeName(x) & ":" & x
    M4856 = R
    Exit Function
EH:
    M4856 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4857() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 0
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(0)
    M4857 = R
    Exit Function
EH:
    M4857 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4858() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 0, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4858 = R
    Exit Function
EH:
    M4858 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4859() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 3
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(3)
    M4859 = R
    Exit Function
EH:
    M4859 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4860() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 3, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4860 = R
    Exit Function
EH:
    M4860 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4861() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 5
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(5)
    M4861 = R
    Exit Function
EH:
    M4861 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4862() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 5, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4862 = R
    Exit Function
EH:
    M4862 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4863() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 6
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(6)
    M4863 = R
    Exit Function
EH:
    M4863 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4864() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 6, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4864 = R
    Exit Function
EH:
    M4864 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4865() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 7
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(7)
    M4865 = R
    Exit Function
EH:
    M4865 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4866() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 7, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4866 = R
    Exit Function
EH:
    M4866 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4867() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 9
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(9)
    M4867 = R
    Exit Function
EH:
    M4867 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4868() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 9, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4868 = R
    Exit Function
EH:
    M4868 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4869() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 11
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(11)
    M4869 = R
    Exit Function
EH:
    M4869 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4870() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 11, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4870 = R
    Exit Function
EH:
    M4870 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4871() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 13
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(13)
    M4871 = R
    Exit Function
EH:
    M4871 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4872() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 13, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4872 = R
    Exit Function
EH:
    M4872 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4873() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 14
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(14)
    M4873 = R
    Exit Function
EH:
    M4873 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4874() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 14, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4874 = R
    Exit Function
EH:
    M4874 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4875() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 16
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(16)
    M4875 = R
    Exit Function
EH:
    M4875 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4876() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 16, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4876 = R
    Exit Function
EH:
    M4876 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4877() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 17
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(17)
    M4877 = R
    Exit Function
EH:
    M4877 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4878() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 17, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4878 = R
    Exit Function
EH:
    M4878 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4879() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 18
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(18)
    M4879 = R
    Exit Function
EH:
    M4879 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4880() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 18, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4880 = R
    Exit Function
EH:
    M4880 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4881() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 20
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(20)
    M4881 = R
    Exit Function
EH:
    M4881 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4882() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 20, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4882 = R
    Exit Function
EH:
    M4882 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4883() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 28
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(28)
    M4883 = R
    Exit Function
EH:
    M4883 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4884() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 28, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4884 = R
    Exit Function
EH:
    M4884 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4885() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 35
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(35)
    M4885 = R
    Exit Function
EH:
    M4885 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4886() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 35, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4886 = R
    Exit Function
EH:
    M4886 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4887() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 47
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(47)
    M4887 = R
    Exit Function
EH:
    M4887 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4888() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 47, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4888 = R
    Exit Function
EH:
    M4888 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4889() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 48
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(48)
    M4889 = R
    Exit Function
EH:
    M4889 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4890() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 48, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4890 = R
    Exit Function
EH:
    M4890 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4891() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 49
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(49)
    M4891 = R
    Exit Function
EH:
    M4891 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4892() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 49, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4892 = R
    Exit Function
EH:
    M4892 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4893() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 51
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(51)
    M4893 = R
    Exit Function
EH:
    M4893 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4894() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 51, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4894 = R
    Exit Function
EH:
    M4894 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4895() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 52
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(52)
    M4895 = R
    Exit Function
EH:
    M4895 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4896() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 52, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4896 = R
    Exit Function
EH:
    M4896 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4897() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 53
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(53)
    M4897 = R
    Exit Function
EH:
    M4897 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4898() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 53, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4898 = R
    Exit Function
EH:
    M4898 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4899() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 54
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(54)
    M4899 = R
    Exit Function
EH:
    M4899 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4900() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 54, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4900 = R
    Exit Function
EH:
    M4900 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4901() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 55
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(55)
    M4901 = R
    Exit Function
EH:
    M4901 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4902() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 55, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4902 = R
    Exit Function
EH:
    M4902 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4903() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 57
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(57)
    M4903 = R
    Exit Function
EH:
    M4903 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4904() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 57, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4904 = R
    Exit Function
EH:
    M4904 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4905() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 58
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(58)
    M4905 = R
    Exit Function
EH:
    M4905 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4906() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 58, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4906 = R
    Exit Function
EH:
    M4906 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4907() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 59
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(59)
    M4907 = R
    Exit Function
EH:
    M4907 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4908() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 59, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4908 = R
    Exit Function
EH:
    M4908 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4909() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 61
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(61)
    M4909 = R
    Exit Function
EH:
    M4909 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4910() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 61, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4910 = R
    Exit Function
EH:
    M4910 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4911() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 62
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(62)
    M4911 = R
    Exit Function
EH:
    M4911 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4912() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 62, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4912 = R
    Exit Function
EH:
    M4912 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4913() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 63
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(63)
    M4913 = R
    Exit Function
EH:
    M4913 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4914() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 63, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4914 = R
    Exit Function
EH:
    M4914 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4915() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 67
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(67)
    M4915 = R
    Exit Function
EH:
    M4915 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4916() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 67, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4916 = R
    Exit Function
EH:
    M4916 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4917() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 68
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(68)
    M4917 = R
    Exit Function
EH:
    M4917 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4918() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 68, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4918 = R
    Exit Function
EH:
    M4918 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4919() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 70
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(70)
    M4919 = R
    Exit Function
EH:
    M4919 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4920() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 70, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4920 = R
    Exit Function
EH:
    M4920 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4921() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 71
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(71)
    M4921 = R
    Exit Function
EH:
    M4921 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4922() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 71, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4922 = R
    Exit Function
EH:
    M4922 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4923() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 74
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(74)
    M4923 = R
    Exit Function
EH:
    M4923 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4924() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 74, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4924 = R
    Exit Function
EH:
    M4924 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4925() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 75
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(75)
    M4925 = R
    Exit Function
EH:
    M4925 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4926() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 75, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4926 = R
    Exit Function
EH:
    M4926 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4927() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 76
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(76)
    M4927 = R
    Exit Function
EH:
    M4927 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4928() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 76, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4928 = R
    Exit Function
EH:
    M4928 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4929() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 91
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(91)
    M4929 = R
    Exit Function
EH:
    M4929 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4930() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 91, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4930 = R
    Exit Function
EH:
    M4930 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4931() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 92
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(92)
    M4931 = R
    Exit Function
EH:
    M4931 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4932() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 92, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4932 = R
    Exit Function
EH:
    M4932 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4933() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 93
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(93)
    M4933 = R
    Exit Function
EH:
    M4933 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4934() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 93, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4934 = R
    Exit Function
EH:
    M4934 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4935() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 94
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(94)
    M4935 = R
    Exit Function
EH:
    M4935 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4936() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 94, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4936 = R
    Exit Function
EH:
    M4936 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4937() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 97
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(97)
    M4937 = R
    Exit Function
EH:
    M4937 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4938() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 97, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4938 = R
    Exit Function
EH:
    M4938 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4939() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 98
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(98)
    M4939 = R
    Exit Function
EH:
    M4939 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4940() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 98, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4940 = R
    Exit Function
EH:
    M4940 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4941() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 100
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(100)
    M4941 = R
    Exit Function
EH:
    M4941 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4942() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 100, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4942 = R
    Exit Function
EH:
    M4942 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4943() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 321
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(321)
    M4943 = R
    Exit Function
EH:
    M4943 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4944() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 321, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4944 = R
    Exit Function
EH:
    M4944 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4945() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 322
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(322)
    M4945 = R
    Exit Function
EH:
    M4945 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4946() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 322, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4946 = R
    Exit Function
EH:
    M4946 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4947() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 325
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(325)
    M4947 = R
    Exit Function
EH:
    M4947 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4948() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 325, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4948 = R
    Exit Function
EH:
    M4948 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4949() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 327
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(327)
    M4949 = R
    Exit Function
EH:
    M4949 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4950() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 327, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4950 = R
    Exit Function
EH:
    M4950 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4951() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 328
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(328)
    M4951 = R
    Exit Function
EH:
    M4951 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4952() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 328, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4952 = R
    Exit Function
EH:
    M4952 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4953() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 335
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(335)
    M4953 = R
    Exit Function
EH:
    M4953 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4954() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 335, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4954 = R
    Exit Function
EH:
    M4954 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4955() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 336
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(336)
    M4955 = R
    Exit Function
EH:
    M4955 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4956() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 336, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4956 = R
    Exit Function
EH:
    M4956 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4957() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 337
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(337)
    M4957 = R
    Exit Function
EH:
    M4957 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4958() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 337, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4958 = R
    Exit Function
EH:
    M4958 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4959() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 338
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(338)
    M4959 = R
    Exit Function
EH:
    M4959 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4960() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 338, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4960 = R
    Exit Function
EH:
    M4960 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4961() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 360
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(360)
    M4961 = R
    Exit Function
EH:
    M4961 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4962() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 360, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4962 = R
    Exit Function
EH:
    M4962 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4963() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 361
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(361)
    M4963 = R
    Exit Function
EH:
    M4963 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4964() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 361, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4964 = R
    Exit Function
EH:
    M4964 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4965() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 380
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(380)
    M4965 = R
    Exit Function
EH:
    M4965 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4966() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 380, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4966 = R
    Exit Function
EH:
    M4966 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4967() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 381
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(381)
    M4967 = R
    Exit Function
EH:
    M4967 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4968() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 381, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4968 = R
    Exit Function
EH:
    M4968 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4969() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 382
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(382)
    M4969 = R
    Exit Function
EH:
    M4969 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4970() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 382, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4970 = R
    Exit Function
EH:
    M4970 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4971() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 383
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(383)
    M4971 = R
    Exit Function
EH:
    M4971 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4972() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 383, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4972 = R
    Exit Function
EH:
    M4972 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4973() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 385
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(385)
    M4973 = R
    Exit Function
EH:
    M4973 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4974() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 385, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4974 = R
    Exit Function
EH:
    M4974 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4975() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 387
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(387)
    M4975 = R
    Exit Function
EH:
    M4975 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4976() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 387, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4976 = R
    Exit Function
EH:
    M4976 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4977() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 393
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(393)
    M4977 = R
    Exit Function
EH:
    M4977 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4978() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 393, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4978 = R
    Exit Function
EH:
    M4978 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4979() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 394
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(394)
    M4979 = R
    Exit Function
EH:
    M4979 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4980() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 394, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4980 = R
    Exit Function
EH:
    M4980 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4981() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 400
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(400)
    M4981 = R
    Exit Function
EH:
    M4981 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4982() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 400, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4982 = R
    Exit Function
EH:
    M4982 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4983() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 402
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(402)
    M4983 = R
    Exit Function
EH:
    M4983 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4984() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 402, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4984 = R
    Exit Function
EH:
    M4984 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4985() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 419
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(419)
    M4985 = R
    Exit Function
EH:
    M4985 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4986() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 419, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4986 = R
    Exit Function
EH:
    M4986 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4987() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 422
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(422)
    M4987 = R
    Exit Function
EH:
    M4987 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4988() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 422, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4988 = R
    Exit Function
EH:
    M4988 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4989() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 423
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(423)
    M4989 = R
    Exit Function
EH:
    M4989 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4990() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 423, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4990 = R
    Exit Function
EH:
    M4990 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4991() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 424
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(424)
    M4991 = R
    Exit Function
EH:
    M4991 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4992() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 424, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4992 = R
    Exit Function
EH:
    M4992 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4993() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 425
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(425)
    M4993 = R
    Exit Function
EH:
    M4993 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4994() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 425, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4994 = R
    Exit Function
EH:
    M4994 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4995() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 429
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(429)
    M4995 = R
    Exit Function
EH:
    M4995 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4996() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 429, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4996 = R
    Exit Function
EH:
    M4996 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4997() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 430
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(430)
    M4997 = R
    Exit Function
EH:
    M4997 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4998() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 430, "Src", "Custom"
    R = Err.Number & "|" & Err.Description & "|" & Err.Source
    M4998 = R
    Exit Function
EH:
    M4998 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M4999() As Variant
    On Error GoTo EH
    Dim R As Variant
    On Error Resume Next
    Err.Raise 432
    R = Err.Number & "|" & Err.Description & "|" & Err.Source & "|" & Err.HelpContext & "|" & Err.LastDllError & "|" & Error(432)
    M4999 = R
    Exit Function
EH:
    M4999 = "#ERR " & Err.Number & " " & Err.Description
End Function
