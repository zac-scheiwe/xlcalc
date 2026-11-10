Attribute VB_Name = "VbaComFso"
Option Explicit

Public Function FsoT1() As String
    Dim f As Object, s As String, p As Variant
    Set f = CreateObject("Scripting.FileSystemObject")
    For Each p In Array("C:\a\b\c.txt", "c.txt", "C:\a\b\", "C:\", "a\b", "C:\a\b.c.d", "\srv\share\x.y", "", "C:\a\..\b", "C:\a\.\b.txt", ".hidden", "dir.d\")
        s = s & "[" & p & "]" & f.GetFileName(p) & "," & f.GetBaseName(p) & "," & f.GetExtensionName(p) & "," & f.GetParentFolderName(p) & "," & f.GetDriveName(p) & ";"
    Next
    FsoT1 = s & TypeName(f)
End Function

Public Function FsoT2() As String
    Dim f As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    s = f.BuildPath("C:\a", "b.txt") & "|" & f.BuildPath("C:\a\", "b.txt") & "|" & f.BuildPath("C:\a", "\b.txt") & "|" & f.BuildPath("", "b") & "|" & f.BuildPath("a", "") & "|" & f.BuildPath("a/", "b")
    s = s & "|" & f.FileExists("C:\no\such.txt") & "|" & f.FolderExists("C:\no\such") & "|" & f.FolderExists("C:\") & "|" & f.DriveExists("C:") & "|" & f.DriveExists("Q:")
    s = s & "|" & f.GetAbsolutePathName("C:\a\..\b") & "|" & f.GetAbsolutePathName("C:\a\.\b\")
    FsoT2 = s
End Function

Public Function FsoT3() As String
    Dim f As Object, s As String
    Set f = CreateObject("Scripting.FileSystemObject")
    On Error Resume Next
    f.GetFileName
    s = "noarg=" & Err.Number: Err.Clear
    f.Nope "x"
    s = s & "|nope=" & Err.Number: Err.Clear
    f.GetFile "C:\no\such.txt"
    s = s & "|getfile=" & Err.Number & ":" & Err.Description: Err.Clear
    f.GetFolder "C:\no\such"
    s = s & "|getfolder=" & Err.Number & ":" & Err.Description: Err.Clear
    s = s & "|tmpname=" & (Len(f.GetTempName()) > 0)
    FsoT3 = s
End Function
