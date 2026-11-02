Attribute VB_Name = "VcLog"
Option Explicit

Public LogS As String

Public Sub Lg(ByVal s As String)
    On Error GoTo EH
    LogS = LogS & s & "|"
    Exit Sub
EH:
    LogS = LogS & "Lg error|"
End Sub

Public Function GetLog() As String
    On Error GoTo EH
    GetLog = LogS
    LogS = ""
    Exit Function
EH:
    GetLog = "GetLog error"
End Function
