# Multiscreen Mega Demo

Go/Ebitengine remake of the original DMA multiscreen mega demo.

## Desktop

```sh
go run ./cmd/multiscreen
```

## Android

The Android launcher runs in immersive sensor-landscape mode. The fixed
800x600 canvas is scaled by Ebitengine to fit the device display.

Requirements: Go, `ebitenmobile`, Android SDK/API 36, Android NDK, JDK 17,
and USB debugging enabled on an authorized device.

Build, install, and launch on the connected device:

```sh
./scripts/run-android.sh
```

Build only the Go Android library:

```sh
./scripts/build-android-aar.sh
```

Build only the debug APK:

```sh
cd android
./gradlew :app:assembleDebug
```

The APK is generated at `android/app/build/outputs/apk/debug/app-debug.apk`.
