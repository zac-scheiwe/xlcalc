Attribute VB_Name = "SolTrace"
Option Explicit

Public LogS As String

Private Function Bits(ByVal x As Double) As String
    Dim neg As Boolean, e As Long, m As Double, hi As Double, lo As Double, h As Long
    If x = 0 Then
        Bits = "0000000000000000"
        Exit Function
    End If
    neg = x < 0
    If neg Then x = -x
    e = 0
    Do While x >= 2
        x = x / 2: e = e + 1
    Loop
    Do While x < 1 And e > -1022
        x = x * 2: e = e - 1
    Loop
    If x >= 1 Then
        m = (x - 1) * (2 ^ 52)
        e = e + 1023
    Else
        m = x * (2 ^ 52) / 2
        e = 0
    End If
    hi = Int(m / (2 ^ 32))
    lo = m - hi * (2 ^ 32)
    h = e
    If neg Then h = h + 2048
    Bits = Right$("000" & Hex$(h), 3) & Right$("00000" & Hex$(hi), 5) & Hex32b(lo)
End Function

Private Function Hex32b(ByVal v As Double) As String
    Dim t As Long
    If v >= (2 ^ 31) Then t = CLng(v - (2 ^ 32)) Else t = CLng(v)
    Hex32b = Right$("0000000" & Hex$(t), 8)
End Function

Public Function Tr(ParamArray v() As Variant) As Double
    On Error GoTo EH
    Dim i As Long
    For i = LBound(v) To UBound(v)
        LogS = LogS & Bits(CDbl(v(i))) & " "
    Next i
    LogS = LogS & "|"
    Tr = 0
    Exit Function
EH:
    LogS = LogS & "TrErr" & Err.Number & "|"
End Function

Public Function GetLog() As String
    GetLog = LogS
    LogS = ""
End Function

Public Function SolShow(ByVal x As Integer) As Long
    LogS = LogS & "CB" & x & "|"
    SolShow = 0
End Function
