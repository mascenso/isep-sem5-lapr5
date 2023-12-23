import { Component, OnInit } from '@angular/core';
import {Router} from "@angular/router";
import UserRole from "../../../../Gestao_Informacao/src/enums/userRole";

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.css']
})
export class RegisterComponent implements OnInit {
  Roles= Object.values(UserRole);
constructor() { }
ngOnInit() {
  }
}