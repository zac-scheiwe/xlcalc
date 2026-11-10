Attribute VB_Name = "VbaDeclare"
Option Explicit

Private Declare PtrSafe Sub Sleep Lib "kernel32" (ByVal ms As Long)
Private Declare PtrSafe Function GetTickCount Lib "kernel32" () As Long
Private Declare PtrSafe Function GetCurrentProcessId Lib "kernel32" Alias "GetCurrentProcessId" () As Long
Private Declare PtrSafe Function NoSuchFn Lib "kernel32" () As Long
Private Declare PtrSafe Function NoSuchLib Lib "nosuchlib_xl.dll" () As Long
Private Declare PtrSafe Function AbsInt Lib "msvcrt" Alias "abs" (ByVal n As Long) As Long

Public Function DeclT1() As String
    Dim t0 As Long, t1 As Long
    On Error Resume Next
    t0 = GetTickCount()
    Sleep 20
    t1 = GetTickCount()
    DeclT1 = TypeName(t0) & "|" & (t1 >= t0) & "|" & (GetCurrentProcessId() > 0) & "|" & Err.Number
End Function

Public Function DeclT2() As String
    Dim s As String
    On Error Resume Next
    Dim x As Long
    x = NoSuchFn()
    s = Err.Number & ":" & Err.Description: Err.Clear
    x = NoSuchLib()
    s = s & "|" & Err.Number & ":" & Err.Description: Err.Clear
    DeclT2 = s
End Function

Public Function DeclT3() As String
    Dim s As String
    On Error Resume Next
    s = AbsInt(-5) & "|" & Err.Number
    DeclT3 = s
End Function
