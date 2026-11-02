Attribute VB_Name = "EvLog"
Option Explicit

Public LogS As String
Public Depth As Long
Public Mode As Long
Public Lim As Long
Public CancelSave As Boolean
Public CancelClose As Boolean
Public AppH As EvApp

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

Public Function SetMode(ByVal m As Long, ByVal n As Long) As Long
    On Error GoTo EH
    Mode = m
    Lim = n
    SetMode = 0
    Exit Function
EH:
    SetMode = Err.Number
End Function

Public Function SetCancel(ByVal save As Boolean, ByVal close_ As Boolean) As Long
    On Error GoTo EH
    CancelSave = save
    CancelClose = close_
    SetCancel = 0
    Exit Function
EH:
    SetCancel = Err.Number
End Function

Public Function InitApp() As Long
    On Error GoTo EH
    Set AppH = New EvApp
    Set AppH.App = Application
    InitApp = 0
    Exit Function
EH:
    InitApp = Err.Number
End Function

Public Function DropApp() As Long
    On Error GoTo EH
    Set AppH = Nothing
    DropApp = 0
    Exit Function
EH:
    DropApp = Err.Number
End Function

Public Sub Ev(ByVal lvl As String, ByVal evn As String, Optional ByVal sh As Object = Nothing, Optional ByVal tgt As Object = Nothing, Optional ByVal extra As String = "")
    On Error GoTo EH
    Dim s As String
    s = String(Depth, ">") & lvl & "." & evn
    If Not sh Is Nothing Then s = s & " " & sh.Name
    If Not tgt Is Nothing Then s = s & " " & tgt.Address(External:=False)
    If Len(extra) > 0 Then s = s & " " & extra
    LogLine s
    Exit Sub
EH:
    LogLine "Ev error " & Err.Number
End Sub

Public Sub Enter(ByVal lvl As String, ByVal evn As String, Optional ByVal sh As Object = Nothing, Optional ByVal tgt As Object = Nothing, Optional ByVal extra As String = "")
    On Error GoTo EH
    Ev lvl, evn, sh, tgt, extra
    Depth = Depth + 1
    Exit Sub
EH:
    LogLine "Enter error " & Err.Number
End Sub

Public Sub Leave()
    On Error GoTo EH
    If Depth > 0 Then Depth = Depth - 1
    Exit Sub
EH:
    LogLine "Leave error " & Err.Number
End Sub

Public Sub Fail(ByVal where_ As String, ByVal num As Long)
    On Error GoTo EH
    LogLine String(Depth, ">") & where_ & " error " & num
    Leave
    Exit Sub
EH:
    LogLine "Fail error " & Err.Number
End Sub

Public Function Safe(ByVal rng As Range) As Boolean
    On Error GoTo EH
    Dim c As Range
    Safe = False
    If rng.Worksheet.ProtectContents Then Exit Function
    If rng.Cells.CountLarge > 2000 Then Exit Function
    For Each c In rng.Cells
        If c.HasArray Then Exit Function
        If InStr(1, c.Formula, "TABLE(", vbTextCompare) > 0 Then Exit Function
        If Not c.ListObject Is Nothing Then
            If Not Application.Intersect(c, c.ListObject.HeaderRowRange) Is Nothing Then Exit Function
        End If
    Next c
    Safe = True
    Exit Function
EH:
    Safe = False
End Function

Public Function SafeWrite(ByVal rng As Range, ByVal v As Variant) As Boolean
    On Error GoTo EH
    SafeWrite = False
    If Not Safe(rng) Then
        LogLine "SafeWrite refused " & rng.Address(External:=False)
        Exit Function
    End If
    rng.Value2 = v
    SafeWrite = True
    Exit Function
EH:
    LogLine "SafeWrite error " & Err.Number
End Function

Public Function SafeFormula(ByVal rng As Range, ByVal f As String) As Boolean
    On Error GoTo EH
    SafeFormula = False
    If Not Safe(rng) Then
        LogLine "SafeFormula refused " & rng.Address(External:=False)
        Exit Function
    End If
    rng.Formula = f
    SafeFormula = True
    Exit Function
EH:
    LogLine "SafeFormula error " & Err.Number
End Function

Public Function SafeFormat(ByVal rng As Range, ByVal f As String) As Boolean
    On Error GoTo EH
    SafeFormat = False
    If Not Safe(rng) Then
        LogLine "SafeFormat refused " & rng.Address(External:=False)
        Exit Function
    End If
    rng.NumberFormat = f
    SafeFormat = True
    Exit Function
EH:
    LogLine "SafeFormat error " & Err.Number
End Function

Public Sub OnChange(ByVal sh As Object, ByVal tgt As Range)
    On Error GoTo EH
    If Depth > Lim Then Exit Sub
    Select Case Mode
    Case 1
        If tgt.Cells.Count = 1 And tgt.Column = 7 Then
            If IsNumeric(tgt.Value2) And Not IsEmpty(tgt.Value2) Then
                LogLine String(Depth, ">") & "write in"
                SafeWrite tgt.Offset(1, 0), tgt.Value2 + 1
                LogLine String(Depth, ">") & "write out"
            End If
        End If
    Case 2
        If tgt.Cells.Count = 1 And tgt.Column = 7 Then
            Application.EnableEvents = False
            LogLine String(Depth, ">") & "write in"
            SafeWrite sh.Range("J1"), Depth
            LogLine String(Depth, ">") & "write out"
            Application.EnableEvents = True
        End If
    Case 3
        If tgt.Cells.Count = 1 And tgt.Column = 7 And sh.Index = 1 Then
            LogLine String(Depth, ">") & "write in"
            SafeWrite ThisWorkbook.Worksheets(2).Range("A1"), tgt.Value2
            LogLine String(Depth, ">") & "write out"
        End If
    Case 4
        If tgt.Cells.Count = 1 And tgt.Column = 7 Then WriteK sh, tgt
    End Select
    Exit Sub
EH:
    LogLine "OnChange error " & Err.Number
    Application.EnableEvents = True
End Sub

Public Sub WriteK(ByVal sh As Object, ByVal tgt As Range)
    On Error GoTo EH
    LogLine String(Depth, ">") & "WriteK in"
    SafeWrite sh.Cells(tgt.Row, 11), tgt.Row * 10
    LogLine String(Depth, ">") & "WriteK out"
    Exit Sub
EH:
    LogLine "WriteK error " & Err.Number
End Sub

Public Sub Auto_Open()
    On Error GoTo EH
    LogLine "Auto_Open"
    Exit Sub
EH:
    LogLine "Auto_Open error " & Err.Number
End Sub

Public Sub Auto_Close()
    On Error GoTo EH
    LogLine "Auto_Close"
    Exit Sub
EH:
    LogLine "Auto_Close error " & Err.Number
End Sub

Public Function Dbl(ByVal x As Variant) As Variant
    On Error GoTo EH
    LogLine "Dbl"
    Dbl = x * 2
    Exit Function
EH:
    Dbl = "err " & Err.Number
End Function
