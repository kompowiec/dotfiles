# Check if the type is already loaded before adding it
if (-not ([System.Management.Automation.PSTypeName]'MouseInput').Type) {
    Add-Type @"
    using System;
    using System.Runtime.InteropServices;

    public class MouseInput {
        [DllImport("user32.dll")]
        public static extern void mouse_event(uint dwFlags, uint dx, uint dy, uint dwData, UIntPtr dwExtraInfo);

        [DllImport("user32.dll")]
        public static extern short GetAsyncKeyState(int vKey);
        
        public const uint MOUSEEVENTF_LEFTDOWN = 0x02;
        public const uint MOUSEEVENTF_LEFTUP   = 0x04;
        public const int VK_Q = 0x51; // Virtual key code for 'Q'
    }
"@
}

function SimulateMouseClick {
    $active = $true
    Write-Host "Mouse simulation active. Press Q to toggle on/off." -ForegroundColor Green

    while ($true) {
        # Check if Q key is pressed to toggle the simulation
        if ([MouseInput]::GetAsyncKeyState([MouseInput]::VK_Q) -lt 0) {
            $active = -not $active
            Write-Host "Mouse simulation " + ($(if ($active) {"activated!"} else {"paused!"})) -ForegroundColor Cyan
            Start-Sleep -Milliseconds 500  # Prevent multiple toggles from a single key press
        }

        # Execute mouse click if active
        if ($active) {
            [MouseInput]::mouse_event([MouseInput]::MOUSEEVENTF_LEFTDOWN, 0, 0, 0, [UIntPtr]::Zero)
            Start-Sleep -Milliseconds 80
            [MouseInput]::mouse_event([MouseInput]::MOUSEEVENTF_LEFTUP, 0, 0, 0, [UIntPtr]::Zero)
            Start-Sleep -Milliseconds 80
        } else {
            Start-Sleep -Milliseconds 50  # Reduce CPU usage while paused
        }
    }
}

# Run the script
SimulateMouseClick

