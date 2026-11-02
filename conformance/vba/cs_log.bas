Attribute VB_Name = "CsLog"
Option Explicit

Public LogS As String
Public N As Long
Public AppH As CsApp

Public Sub LogLine(ByVal s As String)
    On Error GoTo EH
    LogS = LogS & s & "|"
    Exit Sub
EH:
    LogS = LogS & "LogLine error " & Err.Number & "|"
End Sub

Public Function GetLog() As String
    On Error GoTo EH
    GetLog = LogS
    LogS = ""
    Exit Function
EH:
    GetLog = "GetLog error " & Err.Number
End Function

Public Function InitApp() As Long
    On Error GoTo EH
    Set AppH = New CsApp
    Set AppH.App = Application
    InitApp = 0
    Exit Function
EH:
    InitApp = Err.Number
End Function

Public Function CsU(ByVal tag As String, Optional x As Variant) As Variant
    On Error GoTo EH
    Dim v As Variant
    N = N + 1
    If IsMissing(x) Then
        LogLine "U " & tag
    Else
        If TypeName(x) = "Range" Then v = x.Value2 Else v = x
        LogLine "U " & tag & " " & TypeName(v) & ":" & CStr(v)
    End If
    CsU = N
    Exit Function
EH:
    LogLine "CsU error " & Err.Number
    CsU = -1
End Function

Public Function CsC(ByVal tag As String, x As Variant) As Variant
    On Error GoTo EH
    Dim v As Variant
    N = N + 1
    If TypeName(x) = "Range" Then v = x.Value2 Else v = x
    If Not IsEmpty(v) Then LogLine "U " & tag & " " & TypeName(v) & ":" & CStr(v)
    CsC = N
    Exit Function
EH:
    LogLine "CsC error " & Err.Number
    CsC = -1
End Function
