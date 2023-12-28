import { Component } from '@angular/core';
import {Router} from "@angular/router";
import {AuthService} from "../services/auth.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {MatSnackBar} from "@angular/material/snack-bar";

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css']
})
export class LoginComponent {
  hidePassword=true;

  loginForm = new FormGroup({
    email: new FormControl('', [Validators.required, Validators.email]),
    password: new FormControl('', [Validators.required]),
  });

  constructor(private authService: AuthService,
              private router: Router,
              private _snackBar: MatSnackBar
  ) {}

  onSubmit(event:Event){
    event.preventDefault();
    this.onLogin(event);
  }

  onLogin(event:Event) {
    if (!this.loginForm.invalid) {
      this.authService.login(this.loginForm.controls.email.value!, this.loginForm.controls.password.value!)
        .subscribe(
          () => {
            this.router.navigate(['home/profile']);
        },
        (error) => {

          this._snackBar.open("Invalid credentials!", "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      )
    }
  }

  getErrorMessage() {
    if (this.loginForm.controls.email.hasError('required')) {
      return 'You must enter a value';
    }

    return this.loginForm.controls.email.hasError('email') ? 'Not a valid email' : '';
  }

}
