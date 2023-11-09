import { Component } from '@angular/core';
import { LoginService } from '../services/loginService.service';
import {Router} from "@angular/router";

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css'],
  providers: [LoginService]
})
export class LoginComponent {
  email: string = '';
  password: string = '';
  selectedRole: string = '';
  roles: any[] = [];

  constructor(private loginService: LoginService, private router: Router) {}

  ngOnInit() {
    //this.loginService.getRoles().subscribe(roles => {
    //  this.roles = roles;
    //});
    this.roles = ['G_FROTA', 'G_CAMPUS'];
  }

  onSubmit(event:Event,email:string,password:string,role:string){
    event.preventDefault();
    this.loginService.login();
    this.router.navigate(['home']);
  }

  public isLoggedIn(): boolean {
    return this.loginService.isLoggedId();
  }


}
