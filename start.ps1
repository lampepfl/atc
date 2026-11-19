# Start ATC from a Windows checkout. Rebuilds stale jars, loads .env, then runs them through the
# atc.ps1 wrapper, as it runs a release.
$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot
$envFile = if ($env:ATC_ENV_FILE) { $env:ATC_ENV_FILE } else { Join-Path $root '.env' }

# Load simple KEY=value entries without replacing variables inherited from the shell; with a
# name, only that entry.
function Import-EnvFile([string]$Only) {
  if (-not (Test-Path -LiteralPath $envFile -PathType Leaf)) { return }
  foreach ($line in Get-Content -LiteralPath $envFile -Encoding UTF8) {
    if ($line -match '^\s*(?:export\s+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$') {
      $name = $Matches[1]
      if ($Only -and $name -ne $Only) { continue }
      $value = $Matches[2].Trim()
      if ($value.Length -ge 2 -and (($value.StartsWith('"') -and $value.EndsWith('"')) -or
          ($value.StartsWith("'") -and $value.EndsWith("'")))) {
        $value = $value.Substring(1, $value.Length - 2)
      }
      if ($value -and -not [Environment]::GetEnvironmentVariable($name, 'Process')) {
        [Environment]::SetEnvironmentVariable($name, $value, 'Process')
      }
    }
  }
}

$dist = Join-Path $root 'out\dist.dest'
$jar = Join-Path $dist 'atc.jar'
$libJar = Join-Path $dist 'atc-lib.jar'
# Written after each build. Mill leaves the jars alone when the content they are built from
# did not change, so a source only touched or checked out again would stay newer than them
# and make every start rebuild.
$stamp = Join-Path $dist 'build.stamp'
# The build runs before the rest of .env is loaded, so a Mill server it starts does not keep
# the API keys in its environment.
Import-EnvFile 'ATC_SKIP_BUILD'
$needsBuild = $env:ATC_SKIP_BUILD -ne '1' -and (-not (Test-Path -LiteralPath $jar) -or
  -not (Test-Path -LiteralPath $libJar) -or -not (Test-Path -LiteralPath $stamp))

if (-not $needsBuild -and $env:ATC_SKIP_BUILD -ne '1') {
  $builtAt = (Get-Item -LiteralPath $stamp).LastWriteTimeUtc
  # What the distribution is built from; the tests are not part of it.
  $sources = @('build.mill', 'app\src', 'app\resources', 'lib\src') |
    ForEach-Object { Join-Path $root $_ }
  $needsBuild = Get-ChildItem -LiteralPath $sources -File -Recurse |
    Where-Object LastWriteTimeUtc -GT $builtAt | Select-Object -First 1
}

if ($needsBuild) {
  Write-Host '[start.ps1] building distribution (Mill Windows launcher)...' -ForegroundColor DarkGray
  Push-Location -LiteralPath $root
  try {
    & (Join-Path $root 'mill.bat') dist
    if ($LASTEXITCODE -ne 0) { throw "Mill exited with code $LASTEXITCODE" }
    [IO.File]::WriteAllText($stamp, '')
  } finally {
    Pop-Location
  }
}
Import-EnvFile

$argsList = [Collections.Generic.List[string]]::new()
if ($env:ATC_CWD) { $argsList.Add('-C'); $argsList.Add($env:ATC_CWD) }
if ($env:ATC_CONFIG) { $argsList.Add('-c'); $argsList.Add($env:ATC_CONFIG) }
if ($env:ATC_MODEL) { $argsList.Add('-m'); $argsList.Add($env:ATC_MODEL) }
if ($args) { $argsList.AddRange([string[]]$args) }

# The wrapper's functions, without running a wrapper command. Start-Atc takes the -Xmx/-Xms
# arguments for Java and starts it with the JVM options it gives a release.
. (Join-Path $root 'atc.ps1')
Start-Atc $dist ([string[]]$argsList.ToArray())
exit $LASTEXITCODE
