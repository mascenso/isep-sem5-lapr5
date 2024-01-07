describe('Teste de planear tarefas aprovadas', () => {

    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();

        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink=task-planning]').click();

    });



    it('Deve exibir a tabela corretamente e com valores que vai buscar a bd', () => {
        //rota a interceptar para receber o codigo de sucesso
        cy.intercept('POST', 'http://localhost:3000/api/tasks/planning').as('planningRoute');
        cy.get('#mat-input-9').type('6');
        cy.get('#mat-input-10').type('8');
        cy.get('#mat-input-11').type('50');
        cy.get('#mat-input-12').type('25');
        cy.get('#mat-input-13').type('2');
        cy.get('#mat-input-14').type('40');
        cy.get('#mat-input-15').type('5');

        cy.get('.input-section > .mdc-button > .mdc-button__label').click();
        cy.wait('@planningRoute').should(({ response }) => {
            expect(response!.statusCode).to.eq(201);
        });

    });


});