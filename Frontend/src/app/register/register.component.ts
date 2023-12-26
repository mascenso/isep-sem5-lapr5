import { Component, OnInit } from '@angular/core';
import {Router} from "@angular/router";
import UserRole from "../../../../Gestao_Informacao/src/enums/userRole";
import { PrivacyPolicyComponent } from '../../app/privacy-policy/privacy-policy.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.css']
})
export class RegisterComponent implements OnInit {
  Roles= Object.values(UserRole);
  constructor(private dialog: MatDialog) { }
  ngOnInit() {
    }

  openPrivacyPolicyDialog(): void {
    this.dialog.open(PrivacyPolicyComponent, {
      width: '600px', // Set width as needed
      // Optionally configure more settings like height, position, etc.
    });
  }
}