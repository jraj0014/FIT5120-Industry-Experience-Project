# models.py
from django.db import models

class Book(models.Model):
    title = models.CharField(max_length=200)
    pdf_file = models.FileField(upload_to='books/pdfs/')
    cover_image = models.ImageField(upload_to='books/covers/', null=True, blank=True)
    description = models.TextField(null=True, blank=True)

    def __str__(self):
        return str(self.title)
