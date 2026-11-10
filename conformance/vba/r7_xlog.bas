Attribute VB_Name = "XLog"
Option Explicit

Public LogS As String

Public Sub Lg(ByVal s As String)
    LogS = LogS & s & "|"
End Sub

Public Function GetLog() As String
    GetLog = LogS
    LogS = ""
End Function
