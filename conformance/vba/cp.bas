Attribute VB_Name = "CpProbe"
Option Explicit

Public EvS As String
Public Names(0 To 31) As String
Public LogN(0 To 31) As String
Public LogA(0 To 31) As String
Public Cnt(0 To 31) As Long
Public NN As Long
Public AppH As CpApp

Public Sub Ev(ByVal s As String)
    On Error GoTo EH
    EvS = EvS & s & "|"
    Exit Sub
EH:
    EvS = EvS & "Ev error " & Err.Number & "|"
End Sub

Private Function Slot() As Long
    On Error GoTo EH
    Dim nm As String, i As Long
    nm = Application.Caller.Worksheet.Name
    For i = 0 To NN - 1
        If Names(i) = nm Then Slot = i: Exit Function
    Next i
    If NN < 32 Then
        Names(NN) = nm
        NN = NN + 1
        Slot = NN - 1
    End If
    Exit Function
EH:
    Slot = 31
End Function

Public Function CpN(r As Range) As Variant
    On Error GoTo EH
    Dim v As Variant, k As Long
    v = r.Value2
    k = Slot()
    Cnt(k) = Cnt(k) + 1
    LogN(k) = LogN(k) & "[" & Cnt(k) & ":" & TypeName(v) & "]"
    CpN = Cnt(k)
    Exit Function
EH:
    CpN = "err " & Err.Number
End Function

Public Function CpArg(r As Range) As Variant
    On Error GoTo EH
    Dim v As Variant, k As Long
    v = r.Value2
    k = Slot()
    LogA(k) = LogA(k) & "[" & TypeName(v) & ":" & CStr(v) & "]"
    CpArg = v
    Exit Function
EH:
    CpArg = "err " & Err.Number
End Function

Public Function CpInit() As Long
    On Error GoTo EH
    Set AppH = New CpApp
    Set AppH.App = Application
    CpInit = 0
    Exit Function
EH:
    CpInit = Err.Number
End Function

Public Function GetCp() As String
    GetCp = Build(True)
End Function

Public Function GetCpN() As String
    GetCpN = Build(False)
End Function

Private Function Build(ByVal withA As Boolean) As String
    On Error GoTo EH
    Dim i As Long, j As Long, s As String, t As Long
    Dim ord(0 To 31) As Long
    s = "E:" & EvS
    EvS = ""
    For i = 0 To NN - 1
        ord(i) = i
    Next i
    For i = 1 To NN - 1
        For j = i To 1 Step -1
            If StrComp(Names(ord(j)), Names(ord(j - 1)), vbBinaryCompare) < 0 Then
                t = ord(j): ord(j) = ord(j - 1): ord(j - 1) = t
            End If
        Next j
    Next i
    For i = 0 To NN - 1
        j = ord(i)
        If Len(LogN(j)) > 0 Then s = s & " N:" & Names(j) & LogN(j)
        If withA And Len(LogA(j)) > 0 Then s = s & " A:" & Names(j) & LogA(j)
        LogN(j) = ""
        LogA(j) = ""
    Next i
    Build = s
    Exit Function
EH:
    Build = "GetCp error " & Err.Number
End Function
