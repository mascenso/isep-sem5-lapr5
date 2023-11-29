describe('Teste para editar de Elevadores', () => {
    beforeEach(() => {
        //criar edificio para ver na tabela
        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Campus"]').click();

        cy.get('[routerlink="create-building"]').click();
        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-0').type('A' + numeroAleatorio.toString());
        cy.get('#mat-input-1').type('10');
        cy.get('#mat-input-2').type('50');
        cy.get('#mat-input-3').type('Edificio Z');
        cy.get('#mat-input-4').type('TESTE');
        cy.get('.mdc-button__label').click();

        //crio um floor no primeiro building da dropdown
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-floor"]').click();
        cy.intercept('POST', 'http://localhost:4000/api/floors').as('rotaFloor');

        cy.get('#dropdownCreateFloor').click();
        cy.get('.mat-mdc-select-placeholder').click({force: true });

        cy.contains('Edificio Z').click();

        cy.get('#mat-input-5').type("10");
        cy.get('#mat-input-6').type('10');
        cy.get('#mat-input-7').type('1');
        cy.get('#mat-input-8').type('Mais um floor de teste');
        cy.get('#saveNewFloor').click();

        //crio elevadores
        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="create-elevator"]').click();
        cy.intercept('POST', 'http://localhost:4000/api/elevators')

        cy.get('#mat-input-9').type('ELEV' + numeroAleatorio.toString());

        cy.get('#dropdownCreateElevator').click();
        cy.contains('Edificio Z - TESTE').click();

        cy.get('#selectFloorsElevator').click();
        cy.contains('Piso 1').click();
        cy.get('#selectFloorsElevator').type('{esc}');

        cy.get('#saveNewElevator').click({ force: true });

        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="edit-elevators"]').click();
    });

    it('Deve exibir a tabela com os campos editaveis nao vazios', () => {

        cy.get('#dropdownBuildingSelect').click();
        cy.get('.mat-mdc-select-placeholder').click({force: true });

        cy.contains('Edificio Z').click();

        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-21 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('exist');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-22 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('exist');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-23 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('exist');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-21 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('not.be.empty');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-22 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('not.be.empty');
        cy.get('.mat-mdc-form-field.ng-tns-c1205077789-23 > .mat-mdc-text-field-wrapper > .mat-mdc-form-field-flex > .mat-mdc-form-field-infix').should('not.be.empty');

    });

    it('Deve alterar campo code do elev do ultimo elevador criado', () => {

        cy.get('[id^="Campus"]').click();
        cy.get('[routerlink="edit-elevators"]').click();
        cy.intercept('PATCH', 'http://localhost:4000/api/elevators').as('rotaElevator');

        cy.get('#dropdownBuildingSelect').click();
        cy.get('.mat-mdc-select-placeholder').click({force: true });

        cy.contains('Edificio Z').click();

        const numeroAleatorio = Math.floor(1000 + Math.random() * 9000);
        cy.get('#mat-input-10').clear();
        cy.get('#mat-input-10').type('ELEV' + numeroAleatorio.toString());

        cy.get('#saveNewElevator').click({ force: true });

        cy.wait('@rotaElevator').should(({ response }) => {
            expect(response!.statusCode).to.eq(200);
        });

    });
});