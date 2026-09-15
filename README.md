# Multiscreen Mega Demo

Go/Ebitengine remake of the original DMA multiscreen mega demo.

## Desktop

```sh
go run ./cmd/multiscreen
```

## Android

The Android launcher runs in immersive sensor-landscape mode. The fixed
800x600 canvas is scaled by Ebitengine to fit the device display.

Requirements: Go 1.25 or later, Android SDK/API 36, Android NDK, JDK 17,
and USB debugging enabled on an authorized device. The matching `ebitenmobile`
version is pinned by `go.mod` and invoked through `go tool`.

Build, install, and launch on the connected device:

```sh
./scripts/run-android.sh
```

The script detects the connected device ABI and builds only that native
library. This keeps local install APKs small (for example, ARM64 on Pixel).

Build only the Go Android library:

```sh
./scripts/build-android-aar.sh
```

The standalone command builds a universal AAR by default. Select one ABI with,
for example:

```sh
EBITENMOBILE_TARGET=android/arm64 ./scripts/build-android-aar.sh
```

Build only the debug APK:

```sh
cd android
./gradlew :app:assembleDebug
```

The APK is generated at `android/app/build/outputs/apk/debug/app-debug.apk`.
