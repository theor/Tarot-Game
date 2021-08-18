import firebase from "firebase/app";
import "firebase/auth";
import * as firebaseui from 'firebaseui'
import 'firebaseui/dist/firebaseui.css'

import 'firebaseui/dist/firebaseui.css'
var firebaseConfig = {
    apiKey: "AIzaSyBl_ESTJRDm2tN5DzEgjgL3o9jGzfQ_vYE",
    authDomain: "tarot-8b2d4.firebaseapp.com",
    projectId: "tarot-8b2d4",
    storageBucket: "tarot-8b2d4.appspot.com",
    messagingSenderId: "641628880935",
    appId: "1:641628880935:web:1d5ca225a56d4a57aa0e02"
};
// Initialize Firebase
firebase.initializeApp(firebaseConfig);
var ui = new firebaseui.auth.AuthUI(firebase.auth());

export function loginFlow() {
    ui.start('#firebase-auth', {
        signInFlow: 'popup',
        callbacks: {
            signInSuccessWithAuthResult: function (authResult, redirectUrl) {
                // User successfully signed in.
                // Return type determines whether we continue the redirect automatically
                // or whether we leave that to developer to handle.
                return false;
            },
            uiShown: function () {
                // The widget is rendered.
            }
        },
        signInOptions: [
            {
                provider: firebase.auth.GoogleAuthProvider.PROVIDER_ID,
                customParameters: {
                    // Forces account selection even when one account
                    // is available.
                    prompt: 'select_account'
                }
            },
            firebase.auth.EmailAuthProvider.PROVIDER_ID
        ]
    });
}

export function logOut() {
    return firebase.auth().signOut()
}

export function onAuthStateChanged(cb) {
    console.log("onAuthStateChanged", cb)
    
    firebase.auth().onAuthStateChanged(u => {
        console.log("CHANGED", u)
        cb(u);
    }, e => console.log("ERROR", e))
}