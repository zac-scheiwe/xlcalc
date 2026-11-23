Attribute VB_Name = "HlLog"
Option Explicit

Public LogS As String
Public AppH As HlApp

Public Sub L(ByVal s As String)
    LogS = LogS & s & "|"
End Sub

Public Function GetLog() As String
    GetLog = LogS
    LogS = ""
End Function

Public Function InitApp() As Long
    Set AppH = New HlApp
    Set AppH.App = Application
    InitApp = 0
End Function

Public Function Desc(ByVal h As Object) As String
    On Error GoTo EH
    Desc = "[" & h.Address & "][" & h.SubAddress & "][" & h.Type & "]"
    Exit Function
EH:
    Desc = "err " & Err.Number
End Function
