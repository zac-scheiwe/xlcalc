Attribute VB_Name = "EvWB"
Option Explicit

Public LogS As String
Public H As EvWbApp
Public Hk(1 To 9) As EvWbBk

Public Function InitW() As Long
    On Error GoTo EH
    Set H = New EvWbApp
    Set H.App = Application
    InitW = 0
    Exit Function
EH:
    InitW = Err.Number
End Function

Public Function Hook(ByVal i As Long, ByVal nm As String) As Long
    On Error GoTo EH
    Set Hk(i) = New EvWbBk
    Hk(i).Tag = "B" & i
    Set Hk(i).Wb = Workbooks(nm)
    Hook = 0
    Exit Function
EH:
    Hook = Err.Number
End Function

Public Sub Add(ByVal s As String)
    On Error GoTo EH
    LogS = LogS & s & "|"
    Exit Sub
EH:
    LogS = LogS & "Add error|"
End Sub

Public Function GetW() As String
    On Error GoTo EH
    Dim s As String
    Dim i As Long
    s = LogS & "#"
    For i = 1 To Application.Windows.Count
        s = s & " " & Application.Windows(i).Caption
    Next i
    s = s & " active=" & ActiveWorkbook.Name & " win=" & ActiveWindow.Caption
    LogS = ""
    GetW = s
    Exit Function
EH:
    GetW = "GetW error " & Err.Number
End Function
