Attribute VB_Name = "VbaMaths"
Option Explicit

Public Function VExp(x As Double) As Double: VExp = Exp(x): End Function
Public Function VLog(x As Double) As Double: VLog = Log(x): End Function
Public Function VSqr(x As Double) As Double: VSqr = Sqr(x): End Function
Public Function VAtn(x As Double) As Double: VAtn = Atn(x): End Function
Public Function VSin(x As Double) As Double: VSin = Sin(x): End Function
Public Function VCos(x As Double) As Double: VCos = Cos(x): End Function
Public Function VTan(x As Double) As Double: VTan = Tan(x): End Function
Public Function VPow(x As Double, y As Double) As Double: VPow = x ^ y: End Function

Public Function DMul(a As Double, b As Double) As Double: DMul = a * b: End Function
Public Function DDiv(a As Double, b As Double) As Double: DDiv = a / b: End Function
Public Function DAdd(a As Double, b As Double) As Double: DAdd = a + b: End Function
Public Function DMulAdd(a As Double, b As Double, c As Double) As Double: DMulAdd = a * b + c: End Function
Public Function DMulAddStored(a As Double, b As Double, c As Double) As Double
    Dim t As Double: t = a * b: DMulAddStored = t + c
End Function
Public Function VSubCancel(a As Double, b As Double, c As Double) As Double: VSubCancel = (a + b) - c: End Function
Public Function VarMulAdd(a As Variant, b As Variant, c As Variant) As Variant: VarMulAdd = a * b + c: End Function
Public Function VarMul(a As Variant, b As Variant) As Variant: VarMul = a * b: End Function

Public Function VStr(x As Double) As String: VStr = CStr(x): End Function

Public Function X87Checks() As String
    Dim one As Double, e As Double, i As Integer, big As Double, s As String
    one = 1: e = 1
    For i = 1 To 60: e = e / 2: Next
    s = "(1+2^-60)-1=" & CStr((one + e) - one)
    Dim v1 As Variant, v2 As Variant: v1 = one: v2 = e
    s = s & " variant:" & CStr((v1 + v2) - v1)
    big = 1E+308
    On Error Resume Next
    Dim r As Double: r = (big * 10) / 10
    s = s & " (1e308*10)/10=" & IIf(Err.Number = 0, CStr(r), "err" & Err.Number)
    Err.Clear
    Dim t As Double: t = big * 10
    s = s & " stored 1e308*10=" & IIf(Err.Number = 0, CStr(t), "err" & Err.Number)
    X87Checks = s
End Function

Public Function CatchExp(x As Double) As Variant
    Dim r As Double
    On Error Resume Next
    r = Exp(x)
    CatchExp = Array(Err.Number, Err.Description)
End Function
Public Function CatchSqr(x As Double) As Variant
    Dim r As Double
    On Error Resume Next
    r = Sqr(x)
    CatchSqr = Array(Err.Number, Err.Description)
End Function
Public Function CatchLog(x As Double) As Variant
    Dim r As Double
    On Error Resume Next
    r = Log(x)
    CatchLog = Array(Err.Number, Err.Description)
End Function
Public Function CatchPow(x As Double, y As Double) As Variant
    Dim r As Double
    On Error Resume Next
    r = x ^ y
    CatchPow = Array(Err.Number, Err.Description)
End Function
Public Function CatchStoreOverflow() As Variant
    Dim big As Double, t As Double
    big = 1E+308
    On Error Resume Next
    t = big * 10
    CatchStoreOverflow = Array(Err.Number, Err.Description)
End Function
